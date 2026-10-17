#!/bin/bash
# PreToolUse:Bash hook for agents that share a checkout with other sessions. Policy:
# - Any `git` invocation blocks (verdict `git`), whatever the subcommand or flags, read-only ones
#   included, so all version control goes through jj. So does `aitools git ...` and `aitools run|bg`
#   given git as an argument. ALLOW_DESTRUCTIVE_GIT=1 never lifts this verdict.
# - Destructive jj commands block: edit, next, prev, bisect, sparse set/reset/edit, abandon, undo, redo,
#   op restore/revert, restore without paths, and new onto another revision; plus jj git push,
#   which skips Git hooks.
# - jj commands that override the user identity or the private-commit check block (--config user.*/
#   git.private-commits, --config-file, --allow-private, JJ_USER/JJ_EMAIL/JJ_CONFIG).
# Looks through shell wrappers (bash -c, env, env -S, xargs, sudo, eval, ...), shell reserved words
# (`!`, if, then, do, ...), and `jj util exec` to find the underlying command.
# The git verdict fails closed for a literal `git` command word: any path, any arguments, any
# wrapper the hook peels. It does not classify a command name computed at run time (`$g status`,
# `$(echo git) status`, `$'git' status`), a command fed to a shell on stdin (`bash <<< ...`,
# `echo ... | bash`), `coproc git`, wrappers it does not peel (find -exec, watch, ssh, nix
# run/shell), or a wrapper option whose value is a separate word that the hook does not know takes
# one (`xargs -I {} git`, `sudo -T 5 git`, `time -f %e git`).
# Everything else fails open on what the hook cannot classify, except where the unclassifiable part
# could itself be destructive, which blocks: nesting deeper than the recursion limit, a jj
# subcommand computed at run time ($(...), backticks, $VAR), and a jj subcommand that is not a
# built-in (a user alias can expand to anything).
# Input boundary: malformed JSON, an empty command, a non-Bash tool, or no jq and no python3 to parse
# the input all exit 0, since there is nothing to classify.
# Override for the jj verdicts only: ALLOW_DESTRUCTIVE_GIT=1 <command>.

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

# Bound to a variable, not heredoc-fed via $(...), since macOS's bash 3.2 mis-parses a heredoc
# containing a backtick inside $(...).
read -r -d '' perl_prog <<'PERL' || true
use strict;
use warnings;

my $cmd = defined $ENV{HOOK_CMD} ? $ENV{HOOK_CMD} : q{};

# Wrappers whose operand is the real command, e.g. `xargs git log` classifies as `git log`.
my %WRAPPER = map { $_ => 1 } qw(
    sudo doas command builtin exec env nohup timeout nice ionice chrt
    time stdbuf setsid unbuffer xargs rtk
);

# Shell reserved words that precede a command in the same segment (`! cmd`, `then cmd`, `do cmd`).
my %RESERVED = map { $_ => 1 } qw(! if then elif else do while until);

# Wrapper options whose next word is a value, not the command.
my %SUDO_VALUE_OPT = map { $_ => 1 } qw(-u -g -C -D -U -h -p -r -t
    --user --group --close-from --chdir --other-user --host --prompt --role --type --command-timeout --chroot);
my %ENV_VALUE_OPT = map { $_ => 1 } qw(-u --unset -C --chdir);

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
    my $in_backtick = 0;
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
        # data: `cat <<EOF` followed by the words "git stash" must not read as an invocation.
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

        # Command substitution opens a fresh command position. The outer command keeps an opaque
        # `$(` word in its place, so a classifier sees an argument whose value is unknown rather
        # than no argument at all.
        if ($c eq q{$} and substr($s, $i, 2) eq q{$(}) {
            $add->(q{$(}, 0);
            $flush_seg->();
            $i += 2;
            next;
        }

        if ($c eq q{`}) {
            $add->(q{`}, 0) if !$in_backtick;
            $in_backtick = !$in_backtick;
            $flush_seg->();
            $i++;
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

        if ($c =~ /[;\n&|()]/) {
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
# until a real command name is exposed, then classify it if it is git or jj. $allow lifts the jj
# verdicts, never `git` or `depth`: the override exists for a reviewed jj push, not to run git.
sub verdict_for_segment {
    my ($tokens, $depth, $allow) = @_;
    my @t = @{$tokens};

    # Environment that swaps the config jj reads, which can change the identity the push check relies on.
    my $jj_env = 0;
    my $note_env = sub {
        my ($key) = @_;
        $jj_env = 1 if $key =~ /^JJ_(?:USER|EMAIL|CONFIG)$/;
        return;
    };
    my $lift = sub {
        my ($v) = @_;
        return ($allow and $v ne 'git' and $v ne 'depth') ? q{} : $v;
    };

    while (@t) {
        my $raw = $t[0]{text};
        my $w   = $raw;
        $w =~ s{^.*/}{};

        if (!$t[0]{quoted} and $raw =~ /^([A-Za-z_][A-Za-z0-9_]*)=(.*)$/) {
            my ($key, $val) = ($1, $2);
            # The override is honoured where it is written, so it also works one level down
            # inside a shell payload.
            $allow = 1 if $key =~ /^(?:CLAUDE_)?ALLOW_DESTRUCTIVE_GIT$/ and $val eq '1';
            $note_env->($key);
            shift @t;
            next;
        }

        if (!$t[0]{quoted} and $RESERVED{$raw}) { shift @t; next; }

        # eval takes its command as the concatenation of the remaining words.
        if ($w eq 'eval') {
            shift @t;
            return classify(join(q{ }, map { $_->{text} } @t), $depth + 1, $allow);
        }

        if ($SHELL{$w}) {
            shift @t;
            while (@t) {
                my $a = $t[0]{text};
                # -c, and combined short forms such as -lc, take the command string next.
                if ($a =~ /^-[A-Za-z]*c$/) {
                    shift @t;
                    return @t ? classify($t[0]{text}, $depth + 1, $allow) : q{};
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
                # exec -a NAME: NAME is argv[0] for the command that follows, not the command.
                if ($w eq 'exec' and $a eq '-a') { shift @t; shift @t if @t; next; }
                # env -S splits its operand into the command line, so it is re-parsed as one.
                if ($w eq 'env' and $a =~ /^(?:-S|--split-string)=?(.*)$/s) {
                    shift @t;
                    my @cmd = length $1 ? ($1) : @t ? (shift(@t)->{text}) : ();
                    push @cmd, map { shell_quote($_->{text}) } @t;
                    return classify(join(q{ }, @cmd), $depth + 1, $allow);
                }
                # `command -v git` only prints a path.
                return q{} if $w eq 'command' and $a =~ /^-[A-Za-z]*[vV][A-Za-z]*$/;
                if (($w eq 'sudo' and $SUDO_VALUE_OPT{$a}) or ($w eq 'env' and $ENV_VALUE_OPT{$a})) {
                    shift @t; shift @t if @t; next;
                }
                if ($a =~ /^-/) { shift @t; next; }
                if ($a =~ /^([A-Za-z_][A-Za-z0-9_]*)=/) { $note_env->($1); shift @t; next; }
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
    shift @t;
    my @a = map { $_->{text} } @t;

    return $lift->(jj_verdict($depth, $allow, $jj_env, @a)) if $prog eq 'jj';
    return 'git' if $prog eq 'git';
    return aitools_git(@a) ? 'git' : q{} if $prog eq 'aitools';
    return q{};
}

# `aitools git ...` reads repositories; `aitools run|bg start` can launch git itself. The leading
# options may take a value, so the word after one is a candidate for the subcommand too.
sub aitools_git {
    my @a = @_;
    my ($sub, $opt_seen) = (q{}, 0);
    while (@a) {
        my $x = shift @a;
        if ($x =~ /^-/) { $opt_seen = 1; next; }
        return 1 if $x eq 'git';
        $sub = $x;
        last;
    }
    return 1 if $opt_seen and $sub ne q{} and @a and $a[0] eq 'git';
    if ($sub eq 'run' or $sub eq 'bg') {
        for my $x (@a) { (my $b = $x) =~ s{^.*/}{}; return 1 if $b eq 'git'; }
    }
    return 0;
}

# jj options that take a separate value, global or per-subcommand; skipping them keeps a value such
# as `-R .` from reading as a path operand.
my %JJ_VALUE_OPT = map { $_ => 1 } qw(
    -R --repository --at-operation --at-op --color --config --config-file
    -m --message -r --revision -A --insert-after --after -B --insert-before --before
    -f --from -t --into --to -c --changes-in --tool -o --onto
);

# Returns the operands and the options seen, the latter keyed by name (`--opt=value` folded to
# `--opt`) with the option's value, or 1 for a flag.
sub jj_split {
    my @a = @_;
    my (@pos, %opt);
    while (@a) {
        my $x = shift @a;
        if ($x eq '--') { push @pos, @a; last; }
        if ($x =~ /^-/) {
            my ($name, $val) = $x =~ /^([^=]+)(?:=(.*))?$/s;
            $val = shift @a if !defined $val and $JJ_VALUE_OPT{$name} and @a;
            $opt{$name} = defined $val ? $val : 1;
            next;
        }
        push @pos, $x;
    }
    return (\@pos, \%opt);
}

# A word the shell expands at run time (`$(...)`, backticks, `$VAR`) has no value the hook can check.
sub is_dynamic { return $_[0] =~ /[\$`]/; }

sub shell_quote { my ($w) = @_; $w =~ s/'/'\\''/g; return "'$w'"; }

# jj 0.45 built-in subcommands, from `jj --help`, plus the hidden built-in names debug, obslog, and
# evolution-log, and op, the CLI alias of operation. A config alias cannot shadow any of these, so any
# other name may be a user alias expanding to a blocked command. That includes the default config
# aliases b, ci, desc, and st, which a config file can redefine. A jj upgrade that adds a subcommand
# needs it added here, or the hook blocks it.
my %JJ_BUILTIN = map { $_ => 1 } qw(
    abandon absorb arrange bisect bookmark commit config converge describe diff diffedit duplicate
    edit evolog file fix gerrit git help interdiff log metaedit new next operation parallelize prev
    rebase redo resolve restore revert root run show sign simplify-parents sparse split squash status
    tag undo unsign util version workspace
    debug obslog evolution-log op
);

# Config that changes who jj thinks the user is, or which commits it refuses to push, turns the
# `git.private-commits = "~mine()"` author check into a no-op.
sub jj_identity_override {
    my ($jj_env, @args) = @_;
    return 1 if $jj_env;
    while (@args) {
        my $x = shift @args;
        return 1 if $x =~ /^--config-file(?:=|$)/ or $x eq '--allow-private';
        my $val = $x eq '--config' ? shift(@args) : $x =~ /^--config=(.*)$/s ? $1 : undef;
        return 1 if defined $val and $val =~ /^(?:user\.|git\.private-commits)/;
    }
    return 0;
}

# Every jj command snapshots the working copy first, which is harmless; the danger is a command that
# then rewrites the files on disk (moving @ or discarding its changes) under another session, and a
# push, since jj runs no Git hooks and so skips the gitleaks pre-commit scan. A subcommand or
# operand that decides between those and a safe command blocks when it is dynamic.
sub jj_verdict {
    my ($depth, $allow, $jj_env, @args) = @_;
    my ($pos, $opt) = jj_split(@args);
    return q{} if exists $opt->{'-h'} or exists $opt->{'--help'};
    my @p   = @{$pos};
    my $sub = @p ? shift @p : q{};

    return 'jj-identity' if jj_identity_override($jj_env, @args);
    return 'jj-dynamic' if is_dynamic($sub);
    return 'jj-alias' if length $sub and !$JJ_BUILTIN{$sub};
    if ($sub eq 'util' and @p and $p[0] eq 'exec') {
        my @rest = @args;
        shift @rest while @rest and $rest[0] ne 'exec';
        shift @rest;
        shift @rest if @rest and $rest[0] eq '--';
        return classify(join(q{ }, map { shell_quote($_) } @rest), $depth + 1, $allow);
    }
    return "jj-$sub" if $sub =~ /^(?:edit|next|prev|abandon|undo|redo|bisect)$/;
    if ($sub eq 'sparse') {
        return (@p and ($p[0] =~ /^(?:set|reset|edit)$/ or is_dynamic($p[0]))) ? 'jj-sparse' : q{};
    }
    if ($sub eq 'op' or $sub eq 'operation') {
        return (@p and ($p[0] =~ /^(?:restore|revert)$/ or is_dynamic($p[0]))) ? 'jj-op' : q{};
    }
    # Without paths, restore overwrites the working copy (or the --into revision) wholesale.
    return @p ? q{} : 'jj-restore' if $sub eq 'restore';
    if ($sub eq 'new') {
        return q{} if exists $opt->{'--no-edit'};
        return 'jj-new' if grep { exists $opt->{$_} } qw(-A --insert-after --after -B --insert-before --before);
        # -r and -o are aliases for the parent operands.
        my @parents = (@p, map { $opt->{$_} } grep { exists $opt->{$_} } qw(-r --revision -o --onto));
        return (grep { $_ ne '@' } @parents) ? 'jj-new' : q{};
    }
    if ($sub eq 'git') {
        return q{} unless @p;
        return 'jj-push' if is_dynamic($p[0]);
        return ($p[0] eq 'push' and !exists $opt->{'--dry-run'}) ? 'jj-push' : q{};
    }
    return q{};
}

sub classify {
    my ($text, $depth, $allow) = @_;
    # Bounded to 4 levels so a self-referential payload cannot spin. Past that the shell would still
    # run the innermost command, so the hook blocks rather than guess.
    return 'depth' if $depth > 4;
    for my $seg (lex_segments($text)) {
        my $v = verdict_for_segment($seg, $depth, $allow);
        return $v if length $v;
    }
    return q{};
}

my $allow_all = $cmd =~ /^\s*(?:CLAUDE_)?ALLOW_DESTRUCTIVE_GIT=1\s/ ? 1 : 0;
my $verdict = classify($cmd, 0, $allow_all);
print "$verdict\n" if length $verdict;
exit 0;
PERL

verdict="$(HOOK_CMD="$command" perl -e "$perl_prog")"

# An `[[ ... ]] && exit 0` here would leave the whole list returning 1 on the common path,
# which `set -e` turns into a spurious non-zero exit from the hook itself.
if [[ -z $verdict ]]; then
  exit 0
fi

if [[ $verdict == jj-push ]]; then
  cat >&2 <<'EOF'
❌ jj git push blocked (ORCH-P005)

jj runs no Git hooks, so the gitleaks pre-commit scan never saw these commits.
Scan exactly what will be pushed, then push with the override prefix and literal arguments:
  jj git push --dry-run <arguments>     (shows the bookmarks and commits to push)
  gitleaks git --config ~/.config/gitleaks/config.toml --log-opts="<remote>/<bookmark>..<commit>"
                                        (a new bookmark: --log-opts="<commit> --not --remotes")
  ALLOW_DESTRUCTIVE_GIT=1 jj git push <arguments>

A push still needs the user's authorization in the current message.
EOF
  exit 2
fi

if [[ $verdict == jj-* ]]; then
  case "$verdict" in
  jj-edit | jj-next | jj-prev | jj-new | jj-bisect | jj-sparse) detail="This moves the working-copy revision, rewriting the files every session sharing this checkout is editing (jj bisect checks out each revision it tests; jj sparse changes which files exist on disk)." ;;
  jj-abandon) detail="jj abandon drops a revision, including working-copy changes you did not make when it is @." ;;
  jj-restore) detail="jj restore without paths overwrites the working copy (or the --into revision) wholesale, including changes you did not make." ;;
  jj-undo | jj-redo | jj-op) detail="Rewinding the operation log also rewinds the files other sessions are editing." ;;
  jj-dynamic) detail="The subcommand is computed when the shell runs it, so the hook cannot tell whether it is destructive; spell it literally." ;;
  jj-alias) detail="This is not a jj built-in subcommand, so it may be an alias that expands to a destructive one; run the built-in it stands for (status, not st)." ;;
  jj-identity) detail="This overrides the jj user, loads another config, or allows private commits, which disables the check that stops jj git push from sending commits you did not author." ;;
  esac
  cat >&2 <<EOF
❌ Destructive jj operation blocked (ORCH-P005)

$detail
Assume other Claude Code sessions are working in this same checkout right now.

Use instead:
  Start a change       jj new (on top of @; the files stay as they are)
  Park changes         jj describe -m "WIP"; @ already records the working copy
  Undo a revision      jj revert -r <rev> --onto @
  Discard one file     jj restore <path> (allowed)

Still need it? Re-run with the override prefix and tell the user why first:
  ALLOW_DESTRUCTIVE_GIT=1 <your command>
EOF
  exit 2
fi

if [[ $verdict == depth ]]; then
  cat >&2 <<'EOF'
❌ Command nesting too deep (ORCH-P005)

The command nests eval, sh -c, or jj util exec deeper than the hook follows, so what finally runs is
unchecked. Flatten it. ALLOW_DESTRUCTIVE_GIT=1 does not lift this block.
EOF
  exit 2
fi

cat >&2 <<'EOF'
❌ git is disabled for agents (ORCH-P005)

Every git invocation is blocked, read-only ones included, and ALLOW_DESTRUCTIVE_GIT=1 does not lift it.
Use jj instead:
  status              jj status
  diff                jj diff --git
  log                 jj log
  show                jj show --git <rev>
  blame               jj file annotate <file>
  commit              jj commit -m "<message>"
  branch, checkout -b jj bookmark create <b> -r @
  fetch               jj git fetch --remote <r> --branch <b>
  push                jj git push -b <b>  (still gated: see the jj git push block)
  ls-remote           gh api repos/<owner>/<repo>/git/ref/heads/<b> --jq .object.sha
  no .jj/ directory   ask the user before running jj git init --colocate

If git is truly needed, ask the user to run it with:  ! git <arguments>
EOF
exit 2
