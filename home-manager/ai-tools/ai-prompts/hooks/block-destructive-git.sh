#!/bin/bash
# PreToolUse:Bash hook for agents that share a checkout with other sessions. Policy:
# - `git` blocks (verdict `git`) unless its subcommand is on a read-only allowlist: status, log, diff,
#   show, blame, rev-parse, ls-files, ls-tree, ls-remote, merge-base, rev-list, cat-file, grep,
#   describe, shortlog, name-rev, for-each-ref, show-ref, count-objects, check-ignore, plus the
#   read-only forms of remote, config (--get, --get-all), branch, tag, stash list, and worktree list.
#   fetch passes only as `git fetch [safe options] [<named remote> [<refspec>...]]`, where no refspec
#   writes outside refs/remotes/; a URL or path, an option off the safe list (no --tags, --depth,
#   --unshallow, --prune, --force), a tag refspec, a `..` in a refspec, and `main:main` block.
#   Global options before the subcommand (-C, -c, --no-pager, --git-dir, ...) are skipped, but -c/
#   --config-env keys that make git run a program (core.fsmonitor, core.pager, alias.*, diff.*.textconv,
#   man.*, ...), --exec-path=, environment that redirects git's config, repository, or helper programs
#   (GIT_CONFIG_*, GIT_DIR, GIT_PAGER, GIT_SSH*, GIT_TRACE*, ...), and subcommand options that run a
#   program or write a file (--output, --ext-diff, --no-index, -O, ...) block. git accepts unique
#   prefixes of long options, so any `--xxx` of four or more characters that begins one of those
#   options blocks too. Writes go through jj. ALLOW_DESTRUCTIVE_GIT=1 never lifts this verdict.
#   A global option value computed at run time blocks, except the value of -C and --work-tree when it
#   is double-quoted and only a plain variable expansion (`git -C "$repo" log`).
# - `aitools git ...` only reads, so it passes; `aitools run|bg ... git <args>` is judged like `git <args>`.
# - Destructive jj commands block: edit, next, prev, bisect, sparse set/reset/edit, abandon, undo, redo,
#   op restore/revert, restore without paths, and new onto another revision; plus jj git push,
#   which skips Git hooks.
# - jj commands that override the user identity or the private-commit check block (--config user.*/
#   git.private-commits, --config-file, --allow-private, JJ_USER/JJ_EMAIL/JJ_CONFIG).
# Looks through shell wrappers (bash -c, env, env -S, xargs, sudo, eval, ...), shell reserved words
# (`!`, if, then, do, ...), and `jj util exec` to find the underlying command.
# The git verdict fails closed for a literal `git` command word: any path, any arguments, any
# wrapper the hook peels. It does not classify a command name computed at run time (`$g commit`,
# `$(echo git) commit`, `$'git' commit`), a command fed to a shell on stdin (`bash <<< ...`,
# `echo ... | bash`), `coproc git`, wrappers it does not peel (find -exec, watch, ssh, nix
# run/shell), or a wrapper option whose value is a separate word that the hook does not know takes
# one (`xargs -I {} git`, `sudo -T 5 git`, `time -f %e git`). A command substitution inside double
# quotes ("$(git commit)", "`git commit`") is not classified either, for jj as well as git.
# Everything else fails open on what the hook cannot classify, except where the unclassifiable part
# could itself be destructive, which blocks: nesting deeper than the recursion limit, a jj
# subcommand computed at run time ($(...), backticks, $VAR), and a jj subcommand that is not a
# built-in (a user alias can expand to anything).
# Input boundary: malformed JSON, an empty command, a non-Bash tool, or no jq and no python3 to parse
# the input all exit 0, since there is nothing to classify.
# Override for the jj verdicts: ALLOW_DESTRUCTIVE_GIT=1 <command>. It covers jj git push only in the
# default (Codex) mode; with `claude` a push always asks, override or not.
# Argument 1 selects how a jj git push verdict is reported. Without it the hook exits 2 with a banner
# (Codex, which has no interactive approval, and where the override prefix lifts the push block with
# no person confirming). With `claude` it answers permissionDecision "ask" on stdout and exits 0, so
# the person confirms the push after the agent has run the gitleaks scan. A push never hides another
# verdict in the same command line: `jj git push -b x; jj undo` exits 2.
# Every other verdict exits 2 in both modes.

mode="${1:-}"

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
my $CLAUDE_MODE = (defined $ENV{HOOK_MODE} and $ENV{HOOK_MODE} eq 'claude') ? 1 : 0;

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
        $tok = { text => q{}, quoted => 0, udyn => 0 } if !defined $tok;
        $tok->{text} .= $text;
        $tok->{quoted} = 1 if $q;
        # An unquoted `$` or backtick leaves part of the word to the shell's own splitting and globbing.
        $tok->{udyn} = 1 if !$q and $text =~ /[\$`]/;
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
    # Environment that makes a read-only git subcommand run a program.
    my $git_env = 0;
    my $note_env = sub {
        my ($key) = @_;
        $jj_env = 1 if $key =~ /^JJ_(?:USER|EMAIL|CONFIG)$/;
        $git_env = 1 if $key =~ /^GIT_(?:EXTERNAL_DIFF|PAGER|SSH\w*|ASKPASS|EDITOR|SEQUENCE_EDITOR|PROXY_COMMAND|CONFIG_(?:GLOBAL|SYSTEM|COUNT|PARAMETERS|KEY_\d+|VALUE_\d+)|TRACE\w*|DIR|WORK_TREE|EXEC_PATH|TEMPLATE_DIR)$/;
        return;
    };
    # In claude mode a jj push is never lifted: the person confirms it whatever prefix the command carries.
    my $lift = sub {
        my ($v) = @_;
        return q{} if $allow and $v ne 'git' and $v ne 'depth' and !($CLAUDE_MODE and $v eq 'jj-push');
        return $v;
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
    # A dynamic value counts as quoted only when every part of its word is quoted.
    my @aq = map { ($_->{quoted} && !$_->{udyn}) ? 1 : 0 } @t;
    return (git_blocked($git_env, \@aq, @a) ? 'git' : q{}) if $prog eq 'git';
    return aitools_verdict($depth, $allow, @a) if $prog eq 'aitools';
    return q{};
}

# `aitools git ...` only reads (status, log, diff, blame, show). `aitools run|bg` can launch any program,
# so from the first git word after a launcher word the rest is judged as a git command line. Scanning
# stops at the first git word, so `aitools git log --grep run` never reads as a launch.
sub aitools_verdict {
    my ($depth, $allow, @a) = @_;
    my $launcher = 0;
    for my $i (0 .. $#a) {
        $launcher = 1 if $a[$i] eq 'run' or $a[$i] eq 'bg';
        (my $b = $a[$i]) =~ s{^.*/}{};
        next unless $b eq 'git';
        return q{} unless $launcher;
        return classify(join(q{ }, map { shell_quote($_) } @a[$i .. $#a]), $depth + 1, $allow);
    }
    return q{};
}

my %GIT_READONLY = map { $_ => 1 } qw(
    status log diff show blame rev-parse ls-files ls-tree ls-remote merge-base rev-list cat-file
    grep describe shortlog name-rev for-each-ref show-ref count-objects check-ignore version
);

# Global options whose value is the next word when written without `=`.
my %GIT_GLOBAL_VALUE_OPT = map { $_ => 1 } qw(-C -c --git-dir --work-tree --namespace --super-prefix --config-env --attr-source);

# Config keys that make git execute a program, a path, or a different config, so a read-only
# subcommand stops being read-only. remote.* (uploadpack, receivepack, proxy, vcs) and fetch.* (a
# planted fetch.* key rewrites what an allowed `git fetch` does) are covered by the same pattern.
sub git_config_runs_program {
    my ($kv) = @_;
    return $kv =~ /^(?:core\.(?:fsmonitor|pager|editor|sshcommand|askpass|hookspath|gitproxy)|alias\.|pager\.|diff\.external|credential\.|include|url\.|protocol\.|gpg\.|filter\.|uploadpack\.|receive\.|man\.|browser\.|help\.|sequence\.|core\.alternaterefscommand|diff\..*\.(?:textconv|command)|merge\..*\.driver|mergetool\.|difftool\.|http\.proxy|remote\.|fetch\.)/i;
}

# Long options that run a program, write a file, or read outside the repository. git resolves a unique
# prefix of a long option, so an abbreviation of any of these is as dangerous as the full name.
my @GIT_DANGEROUS_LONG = qw(
    --output --output-directory --ext-diff --open-files-in-pager --upload-pack --receive-pack --exec
    --exec-path --delete --move --copy --force --set-upstream-to --unset-upstream --edit-description
    --no-index --textconv
);

# Prefixes shorter than four characters are left alone: they match too many harmless options.
sub long_option_may_be_dangerous {
    for my $x (@_) {
        last if $x eq '--';
        my ($name) = $x =~ /^(--[^=]*)/ or next;
        next if length $name < 4;
        for my $d (@GIT_DANGEROUS_LONG) { return 1 if index($d, $name) == 0; }
    }
    return 0;
}

# fetch updates remote-tracking refs and FETCH_HEAD, never the working tree or a local branch, as long
# as it talks to a configured remote and no refspec names a local branch. So only a named remote and
# plain refspecs pass, and an option must be on the list below. That list is closed on purpose: it
# leaves out --upload-pack, --receive-pack (run a program), --update-head-ok and -u (let fetch
# overwrite the checked-out branch), --prune, -p, --prune-tags, --force, -f (delete or rewrite local
# refs), --tags, -t, and the `tag <name>` and refs/tags/ refspecs (write local tags), --depth,
# --deepen, --unshallow, --shallow-* and --update-shallow (rewrite .git/shallow), --refmap,
# --recurse-submodules, --stdin (refspecs the hook cannot see), and anything
# abbreviated, so a unique-prefix spelling of a dangerous option cannot slip through. Option values
# must be attached with `=`; a separate value would read as the remote.
sub git_fetch_blocked {
    my @a = @_;
    my @pos;
    while (@a) {
        my $x = shift @a;
        if ($x eq '--') { push @pos, @a; last; }
        if ($x !~ /^-/) { push @pos, $x; next; }
        next if $x =~ /^--(?:all|no-tags|quiet|verbose|dry-run|atomic|keep|progress|no-progress|ipv4|ipv6|multiple|refetch|(?:no-)?write-fetch-head|(?:no-)?show-forced-updates|(?:no-)?auto-gc|(?:no-)?auto-maintenance|porcelain)$/;
        next if $x =~ /^--(?:filter|jobs)=[\w.:+-]+$/;
        next if $x =~ /^-[qvnk46]+$/ or $x =~ /^-j\d+$/;
        return 1;
    }
    return 0 unless @pos;
    my $remote = shift @pos;
    return 1 unless $remote =~ /^[A-Za-z0-9_][A-Za-z0-9_.-]*$/;
    for my $r (@pos) {
        # `tag <name>` and refs/tags/ write local tags; `..` could walk a destination out of refs/remotes/.
        return 1 if $r eq 'tag' or $r =~ m{(?:^|[+:])refs/tags/} or $r =~ /\.\./;
        # `src` alone only fills FETCH_HEAD; `src:dst` must land under refs/remotes/.
        return 1 unless $r =~ m{^\+?[\w.\/*\@{}^~-]+(?::refs/remotes/[\w.\/*-]+)?$};
    }
    return 0;
}

# True when the git command line (arguments after `git`) is not provably read-only.
sub git_blocked {
    my ($git_env, $quoted, @a) = @_;
    my @q = @{$quoted};
    return 1 if $git_env;
    while (@a) {
        my $x = $a[0];
        if (is_dynamic($x)) {
            return 1 unless $x =~ /^--work-tree=(.*)$/s and safe_dynamic_path($1, $q[0]);
        }
        last unless $x =~ /^-/;
        shift @a;
        shift @q;
        return 0 if $x eq '--version' or $x eq '--help' or $x eq '-h';
        return 1 if $x =~ /^--exec-path=/;
        my ($name, $val) = $x =~ /^(--[^=]+)=(.*)$/s;
        if (defined $name) {
            return 1 if $name eq '--config-env' and git_config_runs_program($val);
            next;
        }
        if ($GIT_GLOBAL_VALUE_OPT{$x}) {
            return 1 unless @a;
            my $v  = shift @a;
            my $vq = shift @q;
            return 1 if ($x eq '-c' or $x eq '--config-env') and git_config_runs_program($v);
            return 1 if is_dynamic($v) and !(($x eq '-C' or $x eq '--work-tree') and safe_dynamic_path($v, $vq));
        } elsif ($x =~ /^-c(.+)$/s) {
            return 1 if git_config_runs_program($1);
        }
    }
    return 0 unless @a;
    my $sub = shift @a;
    return 1 if is_dynamic($sub);

    # Options that run a program or write a file whatever the subcommand.
    return 1 if grep { /^-[A-Za-z]*O/ } @a;
    return 1 if long_option_may_be_dangerous(@a);

    return git_fetch_blocked(@a) if $sub eq 'fetch';
    return 0 if $GIT_READONLY{$sub};

    my @pos = grep { !/^-/ } @a;
    my %opt = map { $_ => 1 } grep { /^-/ } @a;
    if ($sub eq 'remote') {
        return 0 if !@pos and !grep { !/^(?:-v|--verbose)$/ } keys %opt;
        return (@pos and ($pos[0] eq 'show' or $pos[0] eq 'get-url')) ? 0 : 1;
    }
    if ($sub eq 'config') {
        return 1 if grep { /^(?:--add|--unset|--unset-all|--replace-all|--edit|-e|--rename-section|--remove-section|--file|-f|--blob)(?:=|$)/ } keys %opt;
        return (grep { /^(?:--get|--get-all)$/ } keys %opt) ? 0 : 1;
    }
    if ($sub eq 'branch') {
        return 1 if grep { /^(?:-d|-D|-m|-M|-c|-C|-f|-u|--delete|--move|--copy|--force|--set-upstream-to(?:=.*)?|--unset-upstream|--edit-description|--track|--no-track|-t)$/ } keys %opt;
        return 0 if !@pos;
        return (grep { /^(?:--list|-l|-a|--all|-r|--remotes|--show-current|--contains|--no-contains|--merged|--no-merged|--points-at)(?:=|$)/ } keys %opt) ? 0 : 1;
    }
    if ($sub eq 'tag') {
        return 1 if grep { /^(?:-d|-a|-s|-u|-f|-m|-F|--delete|--annotate|--sign|--force|--local-user|--message|--file)(?:=|$)/ } keys %opt;
        return 0 if !@pos;
        return (grep { /^(?:-l|--list)$/ } keys %opt) ? 0 : 1;
    }
    return ((@pos and $pos[0] eq 'list') ? 0 : 1) if $sub eq 'stash' or $sub eq 'worktree';
    return 1;
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

# A dynamic word is re-emitted unquoted, with its metacharacters escaped, so the re-parse still sees it
# as computed at run time instead of as a quoted literal.
sub shell_quote {
    my ($w) = @_;
    if (is_dynamic($w)) { $w =~ s/([^\w\$])/\\$1/g; return $w; }
    $w =~ s/'/'\\''/g;
    return "'$w'";
}

# The value of -C or --work-tree may be a double-quoted plain variable (`"$repo"`, `"${repo}/sub"`).
# Anything else computed at run time (`$(...)`, backticks, `${x:-y}`) could name any directory.
sub safe_dynamic_path {
    my ($v, $quoted) = @_;
    return 0 unless $quoted;
    (my $rest = $v) =~ s/\$(?:[A-Za-z_]\w*|\{[A-Za-z_]\w*\})//g;
    return $rest !~ /[\$`]/;
}

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
    # A push is reported last: it only asks (claude) or has its own banner, so it must not hide a
    # later segment's verdict, which always blocks.
    my $push = q{};
    for my $seg (lex_segments($text)) {
        my $v = verdict_for_segment($seg, $depth, $allow);
        next unless length $v;
        return $v if $v ne 'jj-push';
        $push = $v;
    }
    return $push;
}

my $allow_all = $cmd =~ /^\s*(?:CLAUDE_)?ALLOW_DESTRUCTIVE_GIT=1\s/ ? 1 : 0;
my $verdict = classify($cmd, 0, $allow_all);
print "$verdict\n" if length $verdict;
exit 0;
PERL

verdict="$(HOOK_CMD="$command" HOOK_MODE="$mode" perl -e "$perl_prog")"

# An `[[ ... ]] && exit 0` here would leave the whole list returning 1 on the common path,
# which `set -e` turns into a spurious non-zero exit from the hook itself.
if [[ -z $verdict ]]; then
  exit 0
fi

if [[ $verdict == jj-push ]]; then
  if [[ $mode == claude ]]; then
    reason='jj git push runs no Git hooks, so the gitleaks pre-commit scan never saw these commits. Scan exactly what will be pushed before confirming: run `jj git push --dry-run <arguments>` to list the bookmarks and commits, then `gitleaks git --config ~/.config/gitleaks/config.toml --log-opts="<remote>/<bookmark>..<commit>"` (a new bookmark: `--log-opts="<commit> --not --remotes"`). Confirm only when the scan is clean.'
    jq -nc --arg r "$reason" '{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "ask", permissionDecisionReason: $r}}'
    exit 0
  fi
  cat >&2 <<'EOF'
❌ jj git push blocked (ORCH-P005)

jj runs no Git hooks, so the gitleaks pre-commit scan never saw these commits.
Scan exactly what would be pushed:
  jj git push --dry-run <arguments>     (shows the bookmarks and commits to push)
  gitleaks git --config ~/.config/gitleaks/config.toml --log-opts="<remote>/<bookmark>..<commit>"
                                        (a new bookmark: --log-opts="<commit> --not --remotes")

This environment cannot ask the user to confirm a push. The override prefix lifts this block without
a person confirming (ALLOW_DESTRUCTIVE_GIT=1 <command>); use it only for a push the user asked for.
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

If this is the only way to do the task, the override prefix lifts this block (Claude Code asks the
user to confirm each use; Codex has no prompt, so the prefix is the only gate there):
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
❌ git write blocked (ORCH-P005)

Only read-only git subcommands are allowed (status, log, diff, show, blame, rev-parse, ls-files, ...),
and ALLOW_DESTRUCTIVE_GIT=1 does not lift this block. Writes go through jj:
  commit              jj commit -m "<message>"
  branch, checkout -b jj bookmark create <b> -r @
  fetch               git fetch <remote> [<ref>...] is allowed (named remote, no local-branch refspec,
                      no --prune or --upload-pack); jj git fetch --remote <r> --branch <b> also works
  push                jj git push -b <b>  (still gated: see the jj git push block)
  clone               ghq get <url>
  no .jj/ directory   run jj git init --colocate in a repo you are working in, or stay on read-only git

Git writes are not available to agents; use the jj form above and continue.
EOF
exit 2
