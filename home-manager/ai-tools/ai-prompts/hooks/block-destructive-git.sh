#!/usr/bin/env bash
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
# - `gh pr merge`, `gh repo delete`, equivalent REST/GraphQL requests, and gh alias set/import
#   block in both modes without an override. Existing aliases and extension exec are not expanded;
#   their bodies are outside this classifier. GraphQL bodies read from files/stdin block.
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
# one (`sudo -T 5 git`, `time -f %e git`). A command substitution inside double
# quotes ("$(git commit)", "`git commit`") is not classified either, for jj as well as git.
# Everything else fails open on what the hook cannot classify, except where the unclassifiable part
# could itself be destructive, which blocks: nesting deeper than the recursion limit, a jj
# subcommand computed at run time ($(...), backticks, $VAR), and a jj subcommand that is not a
# built-in (a user alias can expand to anything).
# Input boundary: malformed JSON, invalid Bash input, and unavailable parsers block. A valid
# non-Bash event or an empty Bash command has nothing to classify and exits 0.
# Override for the jj verdicts: ALLOW_DESTRUCTIVE_GIT=1 <command>. It covers jj git push only in the
# default (Codex) mode.
# Argument 1 selects how a jj git push verdict is reported. Without it the hook exits 2 with a banner
# (Codex, where the override prefix lifts the push block). With `claude` a direct push must pass
# gitleaks first; wrapped pushes block and must be rerun directly. Authorization is governed by the shared prompt. A push never
# hides another verdict in the same command line: `jj git push -b x; jj undo` exits 2.
# Every other verdict exits 2 in both modes.

mode="${1:-}"

set -euo pipefail

input=$(cat)

input_error() {
  echo 'Hook input blocked (ORCH-P005): JSON or Bash command could not be parsed.' >&2
  exit 2
}

if command -v jq &>/dev/null; then
  tool_name=$(printf '%s' "$input" | jq -er 'if type == "object" and (.tool_name | type) == "string" and .tool_name != "" then .tool_name else error("invalid event") end' 2>/dev/null) || input_error
  if [[ $tool_name != Bash ]]; then exit 0; fi
  command=$(printf '%s' "$input" | jq -er 'if (.tool_input | type) == "object" and (.tool_input.command | type) == "string" then .tool_input.command else error("invalid Bash input") end' 2>/dev/null) || input_error
elif command -v python3 &>/dev/null; then
  tool_name=$(printf '%s' "$input" | python3 -c 'import json,sys
d=json.load(sys.stdin)
if not isinstance(d,dict) or not isinstance(d.get("tool_name"),str) or not d["tool_name"]: sys.exit(1)
print(d["tool_name"])' 2>/dev/null) || input_error
  if [[ $tool_name != Bash ]]; then exit 0; fi
  command=$(printf '%s' "$input" | python3 -c 'import json,sys
d=json.load(sys.stdin)
i=d.get("tool_input")
if not isinstance(i,dict) or not isinstance(i.get("command"),str): sys.exit(1)
print(i["command"])' 2>/dev/null) || input_error
else
  echo 'Hook input blocked (ORCH-P005): neither jq nor Python is available.' >&2
  exit 2
fi

if [[ -z $command ]]; then
  exit 0
fi

# Bound to a variable, not heredoc-fed via $(...), since macOS's bash 3.2 mis-parses a heredoc
# containing a backtick inside $(...).
read -r -d '' perl_prog <<'PERL' || true
use strict;
use warnings;
use JSON::PP qw(encode_json);

my $cmd = defined $ENV{HOOK_CMD} ? $ENV{HOOK_CMD} : q{};
my $CLAUDE_MODE = (defined $ENV{HOOK_MODE} and $ENV{HOOK_MODE} eq 'claude') ? 1 : 0;
my @push_argv;

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
my %XARGS_VALUE_OPT = map { $_ => 1 } qw(
    -I -J -L -n -P -s -E -R -S -a -d
    --arg-file --delimiter --max-args --max-procs --max-chars --process-slot-var
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
        my ($text, $q, $literal) = @_;
        $tok = { text => q{}, quoted => 0, udyn => 0, expansion => 0, dynamic => 0 } if !defined $tok;
        $tok->{text} .= $text;
        $tok->{quoted} = 1 if $q;
        # An unquoted `$` or backtick leaves part of the word to the shell's own splitting and globbing.
        $tok->{udyn} = 1 if !$q and $text =~ /[\$`]/;
        $tok->{expansion} = 1 if !$q and $text =~ /[\[\]*?~{}]/;
        $tok->{dynamic} = 1 if !$literal and $text =~ /[\$`]/;
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
            $add->($d, 0, 1);
            next;
        }

        if ($c eq q{'}) {
            my $j = index($s, q{'}, $i + 1);
            $j = $n if $j < 0;
            $add->(substr($s, $i + 1, $j - $i - 1), 1, 1);
            $i = $j + 1;
            next;
        }

        if ($c eq q{"}) {
            my $j   = $i + 1;
            my $buf = q{};
            my $dynamic = 0;
            while ($j < $n) {
                my $d = substr($s, $j, 1);
                last if $d eq q{"};
                if ($d eq q{\\}) { $buf .= substr($s, $j + 1, 1); $j += 2; next; }
                $dynamic = 1 if $d =~ /[\$`]/;
                $buf .= $d;
                $j++;
            }
            $add->($buf, 1, 1);
            $tok->{dynamic} = 1 if $dynamic;
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
    my $wrapped = $depth > 0;

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
    # In claude mode a jj push keeps its verdict, which the push branch below allows.
    my $lift = sub {
        my ($v) = @_;
        return q{} if $allow and $v =~ /^jj-/ and $v ne 'jj-push-wrapper'
            and !($CLAUDE_MODE and $v eq 'jj-push');
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

        if (!$t[0]{quoted} and $RESERVED{$raw}) { $wrapped = 1; shift @t; next; }

        # eval takes its command as the concatenation of the remaining words.
        if ($w eq 'eval') {
            shift @t;
            return classify(join(q{ }, map { $_->{text} } @t), $depth + 1, $allow);
        }

        if ($SHELL{$w}) {
            shift @t;
            while (@t) {
                my $a = $t[0]{text};
                # -c may appear anywhere in a short-option bundle (-lc, -cl, -ec).
                if ($a =~ /^-[A-Za-z]*c[A-Za-z]*$/) {
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
            $wrapped = 1;
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
                if (($w eq 'sudo' and $SUDO_VALUE_OPT{$a}) or ($w eq 'env' and $ENV_VALUE_OPT{$a})
                    or ($w eq 'xargs' and $XARGS_VALUE_OPT{$a})) {
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

    if ($prog eq 'jj') {
        my $v = jj_verdict($depth, $allow, $jj_env, @a);
        return 'jj-push-wrapper' if $CLAUDE_MODE and $wrapped and $v eq 'jj-push';
        return $v if $v eq 'jj-push-wrapper';
        if ($CLAUDE_MODE and $v eq 'jj-push') {
            return 'jj-push-input' if grep { is_dynamic($_->{text}) or $_->{expansion} } @t;
            @push_argv = ($tokens->[0]{text}, @a);
            # Assignments are checked against the original command by the scanner.
            $push_argv[0] = 'jj' if $push_argv[0] =~ /=/;
        }
        return $lift->($v);
    }
    # A dynamic value counts as quoted only when every part of its word is quoted.
    my @aq = map { ($_->{quoted} && !$_->{udyn}) ? 1 : 0 } @t;
    if ($prog eq 'git') {
        my $git_verdict = git_blocked($git_env, \@aq, @a);
        return q{} unless $git_verdict;
        return git_dynamic_path_help($git_env, \@aq, @a) ? 'git-dynamic-path' : 'git';
    }
    return gh_verdict(@t) if $prog eq 'gh';
    return aitools_verdict($depth, $allow, @a) if $prog eq 'aitools';
    return q{};
}

# `aitools git ...` only reads. Its process launchers can invoke protected commands directly
# or through a shell; use the same classifier after finding that command.
sub aitools_verdict {
    my ($depth, $allow, @a) = @_;
    my $launcher = 0;
    for my $i (0 .. $#a) {
        $launcher = 1 if $a[$i] eq 'run' or $a[$i] eq 'bg';
        (my $b = $a[$i]) =~ s{^.*/}{};
        next unless $b eq 'git' or $b eq 'gh' or $b eq 'jj' or $SHELL{$b};
        return q{} unless $launcher;
        return classify(join(q{ }, map { shell_quote($_) } @a[$i .. $#a]), $depth + 1, $allow);
    }
    return q{};
}

# gh permits persistent options before or between subcommands.
sub gh_verdict {
    my @t = @_;
    my @pos;
    while (@t and @pos < 2) {
        my $x = shift(@t)->{text};
        return q{} if $x eq '--help' or $x eq '-h' or $x eq '--version';
        if ($x eq '-R' or $x eq '--repo' or $x eq '--hostname') {
            shift @t if @t;
            next;
        }
        next if $x =~ /^(?:--(?:repo|hostname)=|-R.)/;
        next if $x =~ /^-/;
        push @pos, $x;
        return gh_api_verdict(@t) if @pos == 1 and $x eq 'api';
    }
    return 'gh-destructive' if @pos and is_dynamic($pos[0]);
    return q{} unless @pos == 2;
    return 'gh-destructive' if ($pos[0] eq 'pr' and ($pos[1] eq 'merge' or is_dynamic($pos[1])))
        or ($pos[0] eq 'repo' and ($pos[1] eq 'delete' or is_dynamic($pos[1])))
        or ($pos[0] eq 'alias' and ($pos[1] =~ /^(?:set|import)$/ or is_dynamic($pos[1])));
    return q{};
}

sub gh_api_verdict {
    my @t = @_;
    my ($endpoint, $method, $input);
    my @fields;
    my $operands = 0;
    my %value_opt = map { $_ => 1 } qw(
        -X --method -f --raw-field -F --field --input --hostname --cache
        -H --header -q --jq -p --preview -t --template
    );
    my %flag_opt = map { $_ => 1 } qw(
        -i --include --paginate --slurp --silent --verbose --allow-escape-sequences
    );
    while (@t) {
        my $token = shift @t;
        my $word = $token->{text};
        if (!$operands and $word eq '--') { $operands = 1; next; }
        if (!$operands and $word =~ /^-/) {
            return q{} if $word eq '--help' or $word eq '-h';
            next if $flag_opt{$word};
            my ($option, $value) = $word =~ /^(--[^=]+|-[XfFHqpt])(?:=(.*))?$/s;
            ($option, $value) = ($1, $2) if !defined $option and $word =~ /^(-[XfFHqpt])(.+)$/s;
            return 'gh-destructive' unless defined $option and $value_opt{$option};
            my $dynamic = $token->{dynamic};
            if (!defined $value) {
                return 'gh-destructive' unless @t;
                my $value_token = shift @t;
                $value = $value_token->{text};
                $dynamic = $value_token->{dynamic};
            }
            if ($option eq '-X' or $option eq '--method') {
                return 'gh-destructive' if $dynamic;
                $method = uc $value;
                return 'gh-destructive' unless $method =~ /^(?:GET|HEAD|OPTIONS|POST|PUT|PATCH|DELETE)$/;
            } elsif ($option eq '--input') {
                $input = 1;
            } elsif ($option =~ /^(?:-f|--raw-field|-F|--field)$/) {
                push @fields, [$value, $dynamic];
            }
            next;
        }
        return 'gh-destructive' if defined $endpoint or $token->{dynamic};
        $endpoint = $word;
    }
    return 'gh-destructive' unless defined $endpoint;
    $method //= (@fields or $input) ? 'POST' : 'GET';
    $endpoint =~ s{^https?://[^/]+}{}i;
    $endpoint =~ s/%([0-9a-f]{2})/chr(hex($1))/egi;
    (my $path = $endpoint) =~ s/[?#].*//s;
    $path =~ s{^/}{};
    $path =~ s{^api/v3/}{};
    $path =~ s{/$}{};
    return 'gh-destructive' if $method eq 'PUT' and $path =~ m{^repos/[^/]+/[^/]+/pulls/[^/]+/merge$};
    return 'gh-destructive' if $method eq 'DELETE' and $path =~ m{^repos/[^/]+/[^/]+$};
    if ($path eq 'graphql' or $path eq 'api/graphql') {
        return 'gh-destructive' if $input or $endpoint =~ /\b(?:mergePullRequest|deleteRepository)\b/;
        for my $field (@fields) {
            my ($value, $dynamic) = @{$field};
            my ($key, $body) = split /=/, $value, 2;
            return 'gh-destructive' if is_dynamic($key);
            next unless $key eq 'query';
            return 'gh-destructive' if $dynamic or !defined $body or $body =~ /^\@/
                or $body =~ /\b(?:mergePullRequest|deleteRepository)\b/;
        }
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

sub git_dynamic_path_help {
    my ($git_env, $quoted, @a) = @_;
    return 0 if $git_env;
    my @q = @{$quoted};
    my $found = 0;

    for (my $i = 0; $i < @a;) {
        my $x = $a[$i];
        last unless $x =~ /^-/;
        if ($x =~ /^--work-tree=(.*)$/s) {
            if (is_dynamic($1) and !$q[$i] and safe_dynamic_path($1, 1)) {
                $a[$i] = '--work-tree=repo';
                $q[$i] = 1;
                $found = 1;
            }
            $i++;
            next;
        }
        if ($x eq '-C' or $x eq '--work-tree') {
            return 0 unless defined $a[$i + 1];
            if (is_dynamic($a[$i + 1]) and !$q[$i + 1] and safe_dynamic_path($a[$i + 1], 1)) {
                $a[$i + 1] = 'repo';
                $q[$i + 1] = 1;
                $found = 1;
            }
            $i += 2;
            next;
        }
        $i++;
        $i++ if $GIT_GLOBAL_VALUE_OPT{$x};
    }

    return 0 unless $found;
    return git_blocked($git_env, \@q, @a) ? 0 : 1;
}

# jj options that take a separate value, global or per-subcommand; skipping them keeps a value such
# as `-R .` from reading as a path operand.
my %JJ_VALUE_OPT = map { $_ => 1 } qw(
    -R --repository --at-operation --at-op --color --config --config-file
    -m --message -r --revision -A --insert-after --after -B --insert-before --before
    -f --from -t --into --to -c --changes-in --tool -o --onto --sparse-patterns
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
    if ($sub eq 'workspace' and @p and $p[0] eq 'add') {
        return 'jj-workspace-add' unless exists $opt->{'--sparse-patterns'};
    }
    return q{};
}

sub classify {
    my ($text, $depth, $allow) = @_;
    # Bounded to 4 levels so a self-referential payload cannot spin. Past that the shell would still
    # run the innermost command, so the hook blocks rather than guess.
    return 'depth' if $depth > 4;
    # A push is reported last: it is allowed (claude) or has its own banner, so it must not hide a
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
print encode_json(\@push_argv), "\n" if $CLAUDE_MODE and $verdict eq 'jj-push';
exit 0;
PERL

if ! verdict="$( { HOOK_CMD="$command" HOOK_MODE="$mode" perl -e "$perl_prog"; } 2>/dev/null)"; then
  printf '%s\n' 'Command classification failed (ORCH-P005).' >&2
  exit 2
fi
push_argv=${verdict#*$'\n'}
verdict=${verdict%%$'\n'*}

# An `[[ ... ]] && exit 0` here would leave the whole list returning 1 on the common path,
# which `set -e` turns into a spurious non-zero exit from the hook itself.
if [[ -z $verdict ]]; then
  exit 0
fi

if [[ $verdict == git-dynamic-path ]]; then
  cat >&2 <<'EOF'
❌ Unquoted dynamic git path blocked (ORCH-P005)

Quote the variable expansion so the shell passes the repository path as one argument:
  git -C "$dir" log
  git --work-tree="$dir" status
EOF
  exit 2
fi

if [[ $verdict == gh-destructive ]]; then
  cat >&2 <<'EOF'
GitHub destructive operation blocked (ORCH-P005)

Merge, repository deletion, alias registration, or an unverifiable API request is not available to agents.
Use a literal inspectable request for ordinary API work; ask the user to perform destructive operations.
ALLOW_DESTRUCTIVE_GIT=1 does not lift this block.
EOF
  exit 2
fi

if [[ $verdict == jj-push-wrapper ]]; then
  cat >&2 <<'EOF'
Wrapped jj git push blocked (ORCH-P005)

Run jj git push directly so its revisions can be checked before pushing.
Remove shell and process wrappers such as bash -c, env, or aitools run.
ALLOW_DESTRUCTIVE_GIT=1 does not lift this block.
EOF
  exit 2
fi

if [[ $verdict == jj-push-input ]]; then
  echo 'Unverifiable jj git push arguments blocked (ORCH-P005). Use literal arguments in a direct command.' >&2
  exit 2
fi

if [[ $verdict == jj-push ]]; then
  if [[ $mode == claude ]]; then
    if ! command -v python3 >/dev/null 2>&1; then
      echo 'jj git push blocked (ORCH-P005): Python is unavailable for the secret scan.' >&2
      exit 2
    fi
    scan_status=0
    HOOK_PUSH_ARGV="$push_argv" HOOK_PUSH_COMMAND="$command" HOOK_INPUT="$input" python3 - <<'PY' || scan_status=$?
import fnmatch
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile


def block(reason):
    print("jj git push blocked (ORCH-P005): " + reason, file=sys.stderr)
    raise SystemExit(2)


def run(args, cwd, *, data=None, env=None):
    return subprocess.run(args, cwd=cwd, input=data, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, timeout=60, env=env)


def scan_push():
    argv = json.loads(os.environ["HOOK_PUSH_ARGV"])
    words = shlex.split(os.environ["HOOK_PUSH_COMMAND"])
    while words and re.match(r"^[A-Za-z_]\w*=", words[0]):
        key = words.pop(0).split("=", 1)[0]
        if key not in ("ALLOW_DESTRUCTIVE_GIT", "CLAUDE_ALLOW_DESTRUCTIVE_GIT"):
            block("environment assignments cannot be verified; run the push directly.")
    if words != argv or not argv or os.path.basename(argv[0]) != "jj":
        block("run one literal jj git push command without shell operators.")
    cwd = json.loads(os.environ["HOOK_INPUT"]).get("cwd") or os.getcwd()
    base = [argv[0]]
    selected = {"bookmark": [], "tag": [], "change": [], "revision": [], "remote": []}
    mode = None
    snapshot = True
    positional = []
    value_options = {"-R": "repository", "--repository": "repository", "--remote": "remote",
                     "-b": "bookmark", "--bookmark": "bookmark", "-t": "tag", "--tag": "tag",
                     "-c": "change", "--change": "change", "-r": "revision",
                     "--revision": "revision", "--revisions": "revision", "--color": "color"}
    args = iter(argv[1:])
    for arg in args:
        key, equal, value = arg.partition("=")
        if not equal and len(arg) > 2 and arg[:2] in ("-R", "-b", "-t", "-c", "-r"):
            key, value, equal = arg[:2], arg[2:], "="
        if key in value_options:
            if not equal:
                value = next(args, None)
            if value is None or value == "":
                block("a push option is missing its value.")
            kind = value_options[key]
            if kind == "repository":
                base.extend(["--repository", value])
            elif kind == "color":
                if value not in ("always", "never", "debug", "auto"):
                    block("the color option is invalid.")
            else:
                selected[kind].append(value)
        elif arg in ("--all", "--tracked", "--deleted"):
            if mode is not None and mode != arg:
                block("conflicting push selectors.")
            mode = arg
        elif arg == "--ignore-working-copy":
            snapshot = False
        elif arg in ("--no-pager", "--quiet", "--allow-empty-description"):
            pass
        elif arg.startswith("-"):
            block("unsupported push option; use explicit bookmark or revision selectors.")
        else:
            positional.append(arg)
    if positional != ["git", "push"]:
        block("the push command could not be parsed.")
    explicit = any(selected[k] for k in ("bookmark", "tag", "change", "revision"))
    if mode and explicit:
        block("conflicting push selectors.")
    leak_bin = shutil.which(os.environ.get("GITLEAKS_BIN", "gitleaks"))
    if leak_bin is None:
        block("gitleaks is unavailable.")

    def jj(args, *, pinned=True):
        result = run(base + (["--at-operation", operation] if pinned else [])
                     + ["--ignore-working-copy", "--no-pager", "--color=never"] + args, cwd)
        if result.returncode != 0:
            block("jj could not derive or read the push revisions.")
        return result.stdout.decode("utf-8", errors="surrogateescape")

    # Match the snapshot that the real push will make, then read one consistent operation.
    if snapshot:
        result = run(base + ["--no-pager", "--color=never", "log", "-r", "@", "--no-graph",
                             "-T", 'commit_id ++ "\\n"'], cwd)
        if result.returncode != 0:
            block("the working copy could not be snapshotted for the scan.")
    operation = jj(["op", "log", "--limit", "1", "--no-graph", "-T", 'id ++ "\\n"'], pinned=False).strip()
    if not re.fullmatch(r"[0-9a-f]{128}", operation):
        block("the repository operation could not be identified.")

    def config(name):
        rows = [json.loads(line) for line in jj(["config", "list", "--include-defaults", name,
                                                "-T", 'json(self) ++ "\\n"']).splitlines()]
        rows = [row for row in rows if row["name"] == name and not row["is_overridden"]]
        if len(rows) > 1:
            block("ambiguous push configuration.")
        return rows[0]["value"] if rows else None

    remote_names = []
    for line in jj(["git", "remote", "list"]).splitlines():
        parts = line.split(None, 1)
        if len(parts) != 2:
            block("remote names could not be derived.")
        remote_names.append(parts[0])
    patterns = selected["remote"] or config("git.push")
    if patterns is None:
        patterns = remote_names if len(remote_names) == 1 else ["origin"]
    if isinstance(patterns, str):
        patterns = [patterns]
    if not isinstance(patterns, list) or not patterns or any(not isinstance(p, str) for p in patterns):
        block("push remotes could not be derived.")
    remotes = set()
    for pattern in patterns:
        kind, separator, value = pattern.partition(":")
        if not separator:
            kind, value = "glob", pattern
        if kind not in ("exact", "glob") or any(c in value for c in "|&~()'\""):
            block("unsupported remote pattern; select a literal remote.")
        matches = {name for name in remote_names if (name == value if kind == "exact"
                                                    else fnmatch.fnmatchcase(name, value))}
        if not matches:
            block("the selected remote does not exist.")
        remotes.update(matches)

    def refs(kind, patterns=()):
        rows = [json.loads(line) for line in jj([kind, "list", "--all-remotes", "-T",
                                                'json(self) ++ "\\n"', "--", *patterns]).splitlines()]
        for row in rows:
            if not isinstance(row, dict):
                block("a reference could not be resolved.")
            target = row.setdefault("target", [])
            if not isinstance(target, list) or any(
                    sha is not None and (not isinstance(sha, str)
                                         or not re.fullmatch(r"[0-9a-f]{40}|[0-9a-f]{64}", sha))
                    for sha in target):
                block("a reference could not be resolved.")
            # jj serializes an absent ref as [null]. Keep null terms in multi-term conflicts.
            if target == [None]:
                row["target"] = []
        return rows

    all_refs = {kind: refs(kind) for kind in ("bookmark", "tag")}

    def resolve(revset, *, required=False):
        ids = set(jj(["log", "-r", revset, "--no-graph", "-T", 'commit_id ++ "\\n"']).splitlines())
        if (required and not ids) or any(not re.fullmatch(r"[0-9a-f]{40}|[0-9a-f]{64}", sha) for sha in ids):
            block("push revisions could not be resolved.")
        return ids

    direct = set()
    for revset in selected["change"]:
        direct.update(resolve(revset, required=True))
    revision_ids = set()
    for revset in selected["revision"]:
        revision_ids.update(resolve(revset, required=True))
    named = {kind: set() for kind in ("bookmark", "tag")}
    for kind in ("bookmark", "tag"):
        for pattern in selected[kind]:
            rows = refs(kind, [pattern])
            if not rows:
                block("the selected reference does not exist.")
            named[kind].update(row["name"] for row in rows)
    working_copy = resolve("@", required=True) if snapshot else set()
    commits = set()
    for remote in remotes:
        targets = set(direct)
        default_ids = None
        if not explicit and mode is None:
            expression = config("revsets.git-push")
            if not isinstance(expression, str) or not expression:
                block("the default push revset could not be resolved.")
            # jj substitutes the remote symbol, but not quoted text or the remote= argument name.
            expression = re.sub(r'"(?:\\.|[^"\\])*"|\x27(?:\\.|[^\x27\\])*\x27|\bremote\b(?!\s*=)',
                                lambda m: json.dumps(remote) if m.group() == "remote" else m.group(), expression)
            default_ids = resolve(expression)
        known = set()
        for kind, rows in all_refs.items():
            local = {row["name"]: row["target"] for row in rows if "remote" not in row}
            remote_refs = {row["name"]: row["target"] for row in rows if row.get("remote") == remote}
            tracked = {row["name"] for row in rows if row.get("remote") == remote and "tracking_target" in row}
            # A conflicted remote ref does not establish which ancestors the remote has.
            known.update(sha for ids in remote_refs.values() if len(ids) <= 1 for sha in ids)
            names = set(named[kind])
            if mode == "--all":
                names.update(local)
                names.update(tracked)
            elif mode == "--tracked":
                names.update(tracked)
            elif mode == "--deleted":
                names.update(name for name in tracked if not local.get(name))
            for name, ids in local.items():
                if explicit and set(ids) & revision_ids:
                    names.add(name)
                elif not explicit and mode is None and name in tracked and set(ids) & default_ids:
                    names.add(name)
            for name in names:
                ids = local.get(name, [])
                if len(ids) > 1 or len(remote_refs.get(name, [])) > 1:
                    block("a selected reference is conflicted.")
                targets.update(ids)
        if targets:
            target_expr = "(" + "|".join(sorted(targets)) + ")"
            resolve(target_expr, required=True)
            known_expr = "(" + "|".join(sorted(known)) + ")" if known else "root()"
            remote_commits = resolve(known_expr + ".." + target_expr)
            if remote_commits & working_copy:
                block("the push would snapshot a working-copy target. Run jj commit <paths> -m <message>, then push @-.")
            commits.update(remote_commits)

    def locations(diff):
        mapping = {}
        old_file = new_file = None
        old_line = new_line = None
        for number, line in enumerate(diff.split("\n"), 1):
            if line.startswith("diff --git "):
                old_file = new_file = old_line = new_line = None
            elif old_line is None and line.startswith(("--- ", "+++ ")):
                path = line[4:]
                if path.startswith('"'):
                    path = json.loads(path)
                path = None if path == "/dev/null" else path[2:] if path.startswith(("a/", "b/")) else path
                if line.startswith("--- "):
                    old_file = path
                else:
                    new_file = path
            elif line.startswith("@@ "):
                match = re.match(r"@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@", line)
                if not match:
                    block("diff line locations could not be derived.")
                old_line, new_line = map(int, match.groups())
            elif old_line is not None:
                if line.startswith("+"):
                    mapping[number] = (new_file, new_line)
                    new_line += 1
                elif line.startswith("-"):
                    mapping[number] = (old_file, old_line)
                    old_line += 1
                elif line.startswith(" "):
                    mapping[number] = (new_file, new_line)
                    old_line += 1
                    new_line += 1
        return mapping

    findings = []
    with tempfile.TemporaryDirectory(prefix="jj-push-scan-") as scratch:
        report = os.path.join(scratch, "report.json")
        scan_env = {key: value for key, value in os.environ.items() if not key.startswith("GITLEAKS_")}
        leak_args = [leak_bin, "stdin", "--redact", "--no-banner", "--no-color",
                     "--ignore-gitleaks-allow", "--report-format", "json", "--report-path", report]
        user_config = os.path.expanduser("~/.config/gitleaks/config.toml")
        if os.path.isfile(user_config):
            leak_args.extend(["--config", user_config])
        for commit in sorted(commits):
            diff = jj(["diff", "--git", "--context", "0", "-r", commit])
            mapping = locations(diff)
            # Only additions introduce content. Keep blank metadata/deletion lines so report
            # offsets stay valid; binary diffs contain no additions, as in gitleaks git mode.
            content = "\n".join(line[1:] if number in mapping and line.startswith("+") else ""
                                for number, line in enumerate(diff.split("\n"), 1)) + "\n"
            if os.path.exists(report):
                os.unlink(report)
            result = run(leak_args, scratch, data=content.encode("utf-8", errors="surrogateescape"), env=scan_env)
            if result.returncode not in (0, 1):
                block("gitleaks could not complete the scan.")
            with open(report, encoding="utf-8") as source:
                rows = json.load(source)
            if not isinstance(rows, list) or bool(rows) != (result.returncode == 1):
                block("gitleaks returned an inconsistent report.")
            for row in rows:
                if not isinstance(row, dict):
                    block("gitleaks returned an invalid finding.")
                line = row.get("StartLine")
                rule = row.get("RuleID")
                if not isinstance(line, int) or line not in mapping or not isinstance(rule, str) or not rule:
                    block("gitleaks finding locations could not be verified.")
                path, line = mapping[line]
                if not path or line < 1:
                    block("gitleaks finding locations could not be verified.")
                findings.append({"commit": commit, "file": path, "line": line, "rule": rule})
    if findings:
        print("jj git push blocked (ORCH-P005): secrets detected.", file=sys.stderr)
        for finding in findings:
            print(json.dumps(finding, ensure_ascii=True), file=sys.stderr)
        raise SystemExit(2)
    current = jj(["op", "log", "--limit", "1", "--no-graph", "-T", 'id ++ "\\n"'], pinned=False).strip()
    if current != operation:
        block("the repository changed during the scan; retry the direct push.")


try:
    scan_push()
except (OSError, ValueError, TypeError, KeyError, subprocess.SubprocessError):
    block("the secret scan could not be completed; no push was authorized.")
PY
    if [[ $scan_status -ne 0 ]]; then
      echo 'jj git push blocked (ORCH-P005): secret verification failed.' >&2
      exit 2
    fi
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
  if [[ $verdict == jj-workspace-add ]]; then
    cat >&2 <<'EOF'
❌ jj workspace add blocked (ORCH-P005)

A workspace made without sparse patterns inherits the empty sparse set from the bare repository and checks out no files.
Use this complete form:
  jj -R <repo> workspace add --no-colocate --sparse-patterns full --name <dir> -r <rev> <repo>/.worktrees/<dir>
EOF
    exit 2
  fi
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

If the user has authorized this operation, the override prefix lifts this block without a prompt:
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
  commit              jj commit <paths> -m "<message>"
  branch, checkout -b jj bookmark create <b> -r @
  fetch               git fetch <remote> [<ref>...] is allowed (named remote, no local-branch refspec,
                      no --prune or --upload-pack); jj git fetch --remote <r> --branch <b> also works
  push                jj git push -b <b>  (still gated: see the jj git push block)
  clone               ghq get <url>
  no .jj/ directory   run jj git init --colocate in a repo you are working in, or stay on read-only git

Git writes are not available to agents; use the jj form above and continue.
EOF
exit 2
