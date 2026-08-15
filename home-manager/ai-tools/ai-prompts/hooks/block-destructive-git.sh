#!/bin/bash
# PreToolUse:Bash hook — blocks git commands that mutate shared working-tree state, since
# concurrent sessions may share this checkout: stash (except list/show), switch,
# reset --hard, clean -f, checkout <ref>. Allows checkout -b/-B/--orphan and checkout -- <path>.
# Looks through shell wrappers (bash -c, xargs, sudo, ...) to find the underlying git command,
# and fails open on anything it cannot classify. Override: ALLOW_DESTRUCTIVE_GIT=1 <command>.

set -euo pipefail

input=$(cat)

# Unparsed input yields an empty command, so stand aside rather than error.
if command -v jq &>/dev/null; then
  tool_name=$(echo "$input" | jq -r '.tool_name // ""' 2>/dev/null || echo "")
  command=$(echo "$input" | jq -r '.tool_input.command // ""' 2>/dev/null || echo "")
elif command -v python3 &>/dev/null; then
  tool_name=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("tool_name",""))' 2>/dev/null || echo "")
  command=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("tool_input",{}).get("command",""))' 2>/dev/null || echo "")
else
  # No JSON parser: cannot classify the command, so do not pretend to have checked it.
  exit 0
fi

if [[ $tool_name != "Bash" ]] || [[ -z $command ]]; then
  exit 0
fi

# Bound to a variable, not heredoc-fed via $(...) — macOS's bash 3.2 mis-parses a heredoc
# containing a backtick inside $(...).
read -r -d '' perl_prog <<'PERL' || true
use strict;
use warnings;

my $cmd = defined $ENV{HOOK_CMD} ? $ENV{HOOK_CMD} : q{};

# Wrappers whose operand is the real command, e.g. `xargs git stash` classifies as `git stash`.
my %WRAPPER = map { $_ => 1 } qw(
    sudo doas command builtin exec env nohup timeout nice ionice chrt
    time stdbuf setsid unbuffer xargs rtk
);

# Wrappers whose command arrives as a string operand and has to be re-parsed.
my %SHELL = map { $_ => 1 } qw(bash sh zsh fish dash ksh mksh ash);

# Keeps quotes intact rather than blanking, so a `bash -c` payload can still be re-parsed;
# separators only separate when unquoted.
sub lex_segments {
    my ($s) = @_;
    my @segments;
    my @cur;
    my $tok;
    my @pending;
    my $i = 0;
    my $n = length $s;

    my $flush_tok = sub {
        if (defined $tok) { push @cur, $tok; $tok = undef; }
        return;
    };
    my $flush_seg = sub {
        if (defined $tok) { push @cur, $tok; $tok = undef; }
        push @segments, [@cur] if @cur;
        @cur = ();
        return;
    };
    my $add = sub {
        my ($text, $q) = @_;
        $tok = { text => q{}, quoted => 0 } if !defined $tok;
        $tok->{text} .= $text;
        $tok->{quoted} = 1 if $q;
        return;
    };

    # Read a word from position $i, honouring quotes; used for heredoc delimiters and for
    # redirection targets, neither of which is a command.
    my $skip_word = sub {
        while ($i < $n) {
            my $d = substr($s, $i, 1);
            last if $d =~ /[\s;&|<>()]/;
            if ($d eq q{'} or $d eq q{"}) {
                my $j = index($s, $d, $i + 1);
                $j = $n if $j < 0;
                $i = $j + 1;
                next;
            }
            $i += ($d eq q{\\}) ? 2 : 1;
        }
        return;
    };

    while ($i < $n) {
        my $c = substr($s, $i, 1);

        if ($c eq q{\\}) {
            my $d = ($i + 1 < $n) ? substr($s, $i + 1, 1) : q{};
            $i += 2;
            next if $d eq q{} or $d eq "\n";
            $add->($d, 0);
            next;
        }

        if ($c eq q{'}) {
            my $j = index($s, q{'}, $i + 1);
            $j = $n if $j < 0;
            $add->(substr($s, $i + 1, $j - $i - 1), 1);
            $i = $j + 1;
            next;
        }

        if ($c eq q{"}) {
            my $j   = $i + 1;
            my $buf = q{};
            while ($j < $n) {
                my $d = substr($s, $j, 1);
                last if $d eq q{"};
                if ($d eq q{\\}) { $buf .= substr($s, $j + 1, 1); $j += 2; next; }
                $buf .= $d;
                $j++;
            }
            $add->($buf, 1);
            $i = $j + 1;
            next;
        }

        # Redirections. The operator and its target are not commands, and a heredoc body is
        # data — `cat <<EOF` followed by the words "git stash" must not read as an invocation.
        if ($c eq q{<} or $c eq q{>}) {
            # A leading file descriptor number belongs to the redirection, not to a word.
            $tok = undef if defined $tok and !$tok->{quoted} and $tok->{text} =~ /^\d+$/;
            $flush_tok->();

            if (substr($s, $i, 2) eq q{<<} and substr($s, $i, 3) ne q{<<<}) {
                $i += 2;
                $i++ if substr($s, $i, 1) eq q{-};
                $i++ while $i < $n and substr($s, $i, 1) =~ /[ \t]/;
                my $start = $i;
                $skip_word->();
                my $delim = substr($s, $start, $i - $start);
                $delim =~ s/["'\\]//g;
                push @pending, $delim if length $delim;
                next;
            }

            if (substr($s, $i, 3) eq q{<<<}) { $i += 3; }
            else {
                $i++;
                $i++ if $i < $n and substr($s, $i, 1) =~ /[<>&]/;
            }
            $i++ while $i < $n and substr($s, $i, 1) =~ /[ \t]/;
            $skip_word->();
            next;
        }

        # Command substitution opens a fresh command position.
        if ($c eq q{$} and substr($s, $i, 2) eq q{$(}) {
            $flush_seg->();
            $i += 2;
            next;
        }

        # A brace is only a group delimiter when it stands alone as a word; inside one it is
        # an ordinary character, so `xargs -I{}` does not fracture into two segments.
        if ($c eq '{' or $c eq '}') {
            my $next = ($i + 1 < $n) ? substr($s, $i + 1, 1) : q{ };
            if (defined $tok or $next !~ /[\s;]/) { $add->($c, 0); $i++; next; }
            $flush_seg->();
            $i++;
            next;
        }

        if ($c =~ /[;\n&|()]/ or $c eq q{`}) {
            $flush_seg->();
            $i++;
            $i++ if ($c eq q{&} or $c eq q{|}) and $i < $n and substr($s, $i, 1) eq $c;

            if ($c eq "\n" and @pending) {
                while (@pending) {
                    my $delim = shift @pending;
                    while ($i < $n) {
                        my $eol = index($s, "\n", $i);
                        $eol = $n if $eol < 0;
                        my $line = substr($s, $i, $eol - $i);
                        $i = ($eol < $n) ? $eol + 1 : $n;
                        $line =~ s/^\s+//;
                        $line =~ s/\s+$//;
                        last if $line eq $delim;
                    }
                }
            }
            next;
        }

        if ($c =~ /[ \t\r]/) { $flush_tok->(); $i++; next; }

        $add->($c, 0);
        $i++;
    }

    $flush_seg->();
    return @segments;
}

# Peel environment assignments, wrappers, and shell payloads off the front of a segment
# until a real command name is exposed, then classify it if it is git.
sub verdict_for_segment {
    my ($tokens, $depth) = @_;
    my @t = @{$tokens};

    while (@t) {
        my $raw = $t[0]{text};
        my $w   = $raw;
        $w =~ s{^.*/}{};

        if (!$t[0]{quoted} and $raw =~ /^([A-Za-z_][A-Za-z0-9_]*)=(.*)$/) {
            my ($key, $val) = ($1, $2);
            # The override is honoured where it is written, so it also works one level down
            # inside a shell payload.
            return q{} if $key =~ /^(?:CLAUDE_)?ALLOW_DESTRUCTIVE_GIT$/ and $val eq '1';
            shift @t;
            next;
        }

        # eval takes its command as the concatenation of the remaining words.
        if ($w eq 'eval') {
            shift @t;
            return classify(join(q{ }, map { $_->{text} } @t), $depth + 1);
        }

        if ($SHELL{$w}) {
            shift @t;
            while (@t) {
                my $a = $t[0]{text};
                # -c, and combined short forms such as -lc, take the command string next.
                if ($a =~ /^-[A-Za-z]*c$/) {
                    shift @t;
                    return @t ? classify($t[0]{text}, $depth + 1) : q{};
                }
                if ($a eq '-o' or $a eq '+o' or $a eq '-O') { shift @t; shift @t if @t; next; }
                if ($a =~ /^[-+]/) { shift @t; next; }
                last;
            }
            # No -c: the operand is a script path whose contents this hook cannot see.
            return q{};
        }

        if ($WRAPPER{$w}) {
            shift @t;
            while (@t) {
                my $a = $t[0]{text};
                if ($a =~ /^-/)                            { shift @t; next; }
                if ($a =~ /^[A-Za-z_][A-Za-z0-9_]*=/)      { shift @t; next; }
                # timeout/nice and friends take a numeric operand before the command.
                if ($w =~ /^(?:timeout|nice|ionice|chrt)$/ and $a =~ /^\d+(?:\.\d+)?[smhd]?$/) {
                    shift @t;
                    next;
                }
                last;
            }
            next;
        }

        last;
    }

    return q{} unless @t;

    my $prog = $t[0]{text};
    $prog =~ s{^.*/}{};
    return q{} unless $prog eq 'git';
    shift @t;

    my @a = map { $_->{text} } @t;

    # Global options precede the subcommand; some of them take a separate value.
    while (@a and $a[0] =~ /^-/) {
        my $opt = shift @a;
        shift @a if @a and $opt =~ /^(?:-C|-c|--git-dir|--work-tree|--namespace|--exec-path)$/;
    }
    my $sub = @a ? shift @a : q{};

    if ($sub eq 'stash') {
        my $verb = @a ? $a[0] : q{};
        return q{} if $verb eq 'list' or $verb eq 'show';
        return 'stash';
    }
    if ($sub eq 'switch') {
        return q{} if grep { $_ eq '--help' } @a;
        return 'switch';
    }
    if ($sub eq 'reset') {
        return (grep { $_ eq '--hard' } @a) ? 'reset' : q{};
    }
    if ($sub eq 'clean') {
        return (grep { /^-[A-Za-z]*f/ or $_ eq '--force' } @a) ? 'clean' : q{};
    }
    if ($sub eq 'checkout') {
        return q{} if grep { $_ eq '-b' or $_ eq '-B' or $_ eq '--orphan' or $_ eq '--' } @a;
        return 'checkout';
    }

    return q{};
}

sub classify {
    my ($text, $depth) = @_;
    # Bounded to 4 levels so a self-referential payload cannot spin; falls open past that.
    return q{} if $depth > 4;
    for my $seg (lex_segments($text)) {
        my $v = verdict_for_segment($seg, $depth);
        return $v if length $v;
    }
    return q{};
}

exit 0 if $cmd =~ /^\s*(?:CLAUDE_)?ALLOW_DESTRUCTIVE_GIT=1\s/;

my $verdict = classify($cmd, 0);
print "$verdict\n" if length $verdict;
exit 0;
PERL

verdict="$(HOOK_CMD="$command" perl -e "$perl_prog")"

# An `[[ ... ]] && exit 0` here would leave the whole list returning 1 on the common path,
# which `set -e` turns into a spurious non-zero exit from the hook itself.
if [[ -z $verdict ]]; then
  exit 0
fi

case "$verdict" in
stash) detail="git stash moves your uncommitted work out of the tree another session may be editing." ;;
switch) detail="git switch moves HEAD for every session sharing this checkout." ;;
reset) detail="git reset --hard discards uncommitted work irrecoverably, including work you did not make." ;;
clean) detail="git clean -f deletes untracked files irrecoverably, including another session's scratch files." ;;
checkout) detail="git checkout <ref> moves HEAD for every session sharing this checkout." ;;
*) detail="This command mutates shared working-tree state." ;;
esac

cat >&2 <<EOF
❌ Destructive Git operation blocked (ORCH-P005)

$detail
Assume other Claude Code sessions are working in this same checkout right now.

Use instead:
  Isolate a branch     git worktree add -b feat/<name> "\$(d=\$(git rev-parse --path-format=absolute --git-common-dir); echo "\${d%/.git}")/.worktrees/<timestamp>-<sha>" origin/<default>
  Park changes         git commit -m "WIP" (on your own branch) — not git stash
  Undo a commit        git revert <sha>, or git reset --soft HEAD~1 — not --hard
  Discard one file     git checkout -- <path> (allowed)
  New branch           git checkout -b <name> (allowed)

Still need it? Re-run with the override prefix and tell the user why first:
  ALLOW_DESTRUCTIVE_GIT=1 <your command>
EOF
exit 2
