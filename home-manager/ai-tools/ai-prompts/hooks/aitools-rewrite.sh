#!/bin/bash
# PreToolUse:Bash hook that rewrites a read-only shell command (cat, head, tail, sed -n, grep, rg,
# ls, find, fd, tree, wc -c, wc -l, jq, diff) into the equivalent
# `aitools` call, so the model gets aitools' JSON with line numbers, hashes, and next_commands
# instead of raw text.
#
# The first argument selects the output shape:
#   claude (default)  hookSpecificOutput.updatedInput only. A permissionDecision would auto-approve
#                     the command sight-unseen.
#   codex             Codex rejects updatedInput unless permissionDecision is "allow", and that approves
#                     the whole command, untouched segments included. So output is emitted only when
#                     every segment was rewritten into a reading aitools command or is a plain
#                     `cd DIR`; any other segment (`cat f; rm -rf x`) makes the hook print nothing.
#   plain             stdin is the raw command; stdout is the rewrite, or empty for no rewrite. Used
#                     by the opencode plugin.
#
# The command is a list of simple commands joined by `;` or `&&`, each optionally wrapped in the
# envelopes agents add most: a trailing `2>&1` or `2>/dev/null`, and a trailing `| head -N` or
# `| tail -N`, which becomes aitools' own line or result limit where aitools can count the same
# units (not for tree output, rg -o, or a search with context lines). Each segment is rewritten or
# kept as typed on its own; a plain `cd DIR` segment stays and sets the directory for the later ones.
# A `cd` joined to the next segment by `;` leaves the directory unknown (it may have failed), and so
# do a `cd` with an option, `~user`, or more than one operand, and pushd, popd, source, `.`, eval, and
# exec: later segments stay as typed. So does any `~` after a segment that assigns, exports, or unsets
# HOME. A list with `||`, `&`, a group, a loop or `if`, a heredoc, a substitution, a comment, a
# backslash, or a newline stays whole as typed. Any other pipeline or redirect feeds the output to
# another program, and aitools' JSON would break it.
#
# Exit statuses that differ from the shell command: grep, rg, and diff exit 1 on no match or a
# difference where aitools exits 0 (unless a trailing head/tail makes head's status the pipeline's),
# jq prints null and exits 0 for a missing key where `aitools json get` exits 1, and a read piped to
# head/tail of a file that is not a regular file reports head's 0 in the shell. Such a segment is
# rewritten only as the last segment of the whole command; before any `;` or `&&` it stays as typed.
# An empty file, or a line range that starts beyond the last line, is left to the shell command, since
# `aitools read` fails on both (see is_empty_file and range_past_eof).
# A flag outside the per-command allowlist, an unquoted glob, a variable, or a regex whose dialect
# differs from aitools' leaves the command untouched. Everything fails open. To keep a command as
# typed, prefix it: `command cat f`.
#
# rg, fd, and tree skip hidden entries where aitools lists them, so their rewrites add globs that
# exclude dot-files and dot-directories. Those globs match workspace-relative paths, so the command
# stays as typed when a start point or the `cd DIR` is hidden, absolute, home-relative, or uses `..`,
# or when the working directory sits below a dot-directory of the workspace. rg --hidden, tree -a,
# and fd -H -I also stay as typed, since they list .git, which aitools always skips. rg -g is
# rewritten only to exclude an extension (`!*.EXT`): any other rg glob also matches directories,
# and a selecting one brings back gitignored files, neither of which an aitools glob does.
#
# `aitools search` and `aitools find` reject a start point outside the workspace root (argument.invalid),
# while read, info, json get, and diff accept any path. So grep, rg, ls, find, fd, and tree stay as
# typed when a path operand, or the `cd DIR` target, resolves outside the root (nearest ancestor of the
# working directory holding a `.git` entry): an absolute path elsewhere, `..` that climbs out, or a
# `~` path (an unquoted `~` is expanded to the home directory first, which is outside the root).
#
# `~` handling: an unquoted `~` or `~/x` operand, or `cd ~` and `cd ~/x`, expands to HOME (which must
# be an absolute path). `~user`, a quoted tilde, and a tilde inside a word stay as typed.
#
# wc -c FILE becomes `aitools info`. wc -l FILE does too, only when the file exists and is empty or
# ends in a newline, since info's line count also counts an unterminated last line.
#
# A grep pattern in the default (basic) dialect is converted to aitools' regex: \| \( \) \{ \} \+ \?
# become the bare operators. A pattern that holds a bare + ? | ( ) { }, an escaped letter or digit,
# a `$` or `^` outside an anchoring position, `\{,`, a quantifier after a quantifier, or an empty
# alternation branch or group stays as typed.
#
# jq is rewritten only for `.` or a chain of `.name` and `[N]` steps. On a missing key `aitools json
# get` fails with input.not-found and lists the pointers that do exist, where jq prints null.

set -euo pipefail

mode="${1:-claude}"
input=$(cat)

# Cheap prefilter, so the common non-rewritable command (jj, nix, git, ...) never pays for perl. It
# only has to be a superset of what the rewriter accepts: a rewritable command word at the start (after
# an optional `cd DIR &&`) or right after a `;` or `&`. A miss only skips the rewrite.
cmd_words='(cat|head|tail|sed|grep|rg|ls|find|fd|tree|wc|jq|diff)'
cd_env='(cd[[:space:]]+[A-Za-z0-9_/.,:@%+~-]+[[:space:]]*(&|\\u0026)(&|\\u0026)[[:space:]]*)?'
if [[ $mode == plain ]]; then
  prefilter="^[[:space:]]*${cd_env}${cmd_words}([[:space:]]|\$)"
else
  prefilter="\"command\"[[:space:]]*:[[:space:]]*\"[[:space:]]*${cd_env}${cmd_words}([[:space:]]|\\\\|\")"
fi
# Later segments of a `;` or `&&` list are rewritten too, so a rewritable word right after a separator
# also passes.
after_sep="(;|&|u0026)[[:space:]]*${cmd_words}([[:space:]]|\\\\|\")"
[[ $input =~ $prefilter || $input =~ $after_sep ]] || exit 0

if ! command -v aitools &>/dev/null || ! command -v perl &>/dev/null; then
  exit 0
fi

# Bound to a variable, not heredoc-fed via $(...), since macOS's bash 3.2 mis-parses a heredoc
# containing a backtick inside $(...).
read -r -d '' perl_prog <<'PERL' || true
use strict;
use warnings;
use JSON::PP;
use Cwd qw(getcwd abs_path);

my ($mode, $input) = @ARGV;

my ($command, $tool_input, $cwd);
if ($mode eq 'plain') {
  $command = $input;
} else {
  my $payload = eval { JSON::PP->new->decode($input) };
  exit 0 unless ref $payload eq 'HASH';
  exit 0 unless ($payload->{tool_name} // '') eq 'Bash';
  my $event = $payload->{hook_event_name} // '';
  exit 0 if $event ne '' && $event ne 'PreToolUse';
  $tool_input = $payload->{tool_input};
  exit 0 unless ref $tool_input eq 'HASH';
  $command = $tool_input->{command};
  $cwd = $payload->{cwd} if defined $payload->{cwd} && !ref $payload->{cwd};
}
exit 0 unless defined $command && !ref $command;
$cwd //= getcwd();

$command =~ s/\A\s+//;
$command =~ s/\s+\z//;
my @segments = split_chain($command) or exit 0;
my ($cd_dir, $cd_known, $home_known, $changed, $all_handled, $new) = (undef, 1, 1, 0, 1, '');
for my $i (0 .. $#segments) {
  my ($text, $sep) = @{ $segments[$i] };
  my $plain_cd = $text =~ m{\Acd\s+([A-Za-z0-9_/.,:\@%+~-]+)\z} ? $1 : undef;
  $plain_cd = undef if defined $plain_cd && !cd_dir_ok($plain_cd, $home_known);
  my $handled = 0;
  if (defined $plain_cd) {
    # A plain `cd DIR` stays as typed; later segments resolve their operands against it.
    $cd_dir = ($plain_cd =~ m{\A[/~]} || !defined $cd_dir) ? $plain_cd : "$cd_dir/$plain_cd";
    # After `cd DIR ;` the next segment runs even when the cd failed.
    $cd_known = 0 if $sep =~ /;/;
    $handled = 1;
  } elsif ($text =~ /\A(?:cd|pushd|popd|source|\.|eval|exec)(?:\s|\z)/) {
    $cd_known = 0;
  } elsif ($text =~ /(?:\A|\s)HOME=/ || $text =~ /\A(?:export|declare|typeset|readonly|local|unset)\s.*\bHOME\b/) {
    $home_known = 0;
  } elsif ($cd_known && ($home_known || $text !~ /~/)) {
    my $out = rewrite_segment($text, $cwd, $cd_dir, $i == $#segments);
    if (defined $out) { $text = $out; $changed = 1; $handled = 1 }
  }
  $all_handled = 0 unless $handled;
  $new .= $text . $sep;
}
exit 0 unless $changed;
# Codex's "allow" approves the whole command, so every segment must be known-safe.
exit 0 if $mode eq 'codex' && !$all_handled;

if ($mode eq 'plain') {
  print $new;
  exit 0;
}
my %updated = (%$tool_input, command => $new);
my %out = (hookEventName => 'PreToolUse', updatedInput => \%updated);
$out{permissionDecision} = 'allow' if $mode eq 'codex';
print JSON::PP->new->canonical->encode({ hookSpecificOutput => \%out });
exit 0;

# A `cd` operand the hook can resolve: not an option, and a tilde only as `~` or `~/...` with a usable HOME.
sub cd_dir_ok {
  my ($dir, $home_known) = @_;
  return 0 if $dir =~ /\A-/;
  return 1 unless $dir =~ /~/;
  return $home_known && ($ENV{HOME} // '') =~ m{\A/} && $dir =~ m{\A~(?:/|\z)} && $dir !~ m{.~};
}

# Rewrites one simple command (with its trailing stderr redirect and head/tail filter), or returns
# undef to keep it as typed. A rewrite whose exit status can differ from the shell command's is
# only made for the last segment of the whole command.
sub rewrite_segment {
  my ($text, $cwd, $cd_dir, $is_last) = @_;
  my ($body, $redirect, $filter) = split_envelope($text);
  my @words = split_words($body) or return undef;
  my %ctx = (cd_dir => $cd_dir, cwd => $cwd, hidden_cwd => under_dot_directory($cwd));
  my @rewritten = rewrite(\%ctx, @words) or return undef;
  # Every `aitools read` selector fails with selection.no-match (exit 2) on a file without lines, where
  # cat, head, tail, and sed succeed with no output. A range starting past the last line fails the same way.
  return undef if $rewritten[0] eq 'read' && is_empty_file($rewritten[1], $cwd, $cd_dir);
  return undef if $rewritten[0] eq 'read' && range_past_eof(\@rewritten, $cwd, $cd_dir);
  my $differs = $rewritten[0] eq 'json'
    || (!$filter && $rewritten[0] =~ /\A(?:search|diff)\z/)
    || ($filter && $rewritten[0] eq 'read' && !-f resolve_path($rewritten[1], $cwd, $cd_dir));
  return undef if $differs && !$is_last;
  if ($filter) {
    @rewritten = apply_filter($filter, @rewritten) or return undef;
  }
  return join(' ', map { quote($_) } 'aitools', @rewritten) . $redirect;
}

# Splits the command at top-level `;` and `&&` into [text, separator] pairs, keeping the separator and
# its whitespace as typed. Returns () when the text holds anything that could hide or move a
# separator (escapes, substitutions, heredocs, groups, comments, `||`, `&`, `|&`, newlines) or a
# compound-command keyword, so the whole command stays as typed.
sub split_chain {
  my ($s) = @_;
  return () if $s eq '';
  my @chars = split //, $s;
  my (@out, $state, $start);
  ($state, $start) = ('', 0);
  my $n = @chars;
  for (my $i = 0; $i < $n; $i++) {
    my $c = $chars[$i];
    my $next = $i + 1 < $n ? $chars[$i + 1] : '';
    my $prev = $i > 0 ? $chars[$i - 1] : '';
    return () if $c eq "\n" || $c eq '`' || ($c eq '$' && $next =~ /[('"]/);
    if ($state eq "'") {
      $state = '' if $c eq "'";
    } elsif ($state eq '"') {
      if ($c eq '"') { $state = '' } elsif ($c eq '\\') { $i++ }
    } elsif ($c eq "'" || $c eq '"') {
      $state = $c;
    } elsif ($c =~ /[\\(){}]/) {
      return ();
    } elsif ($c eq '#' && ($i == 0 || $prev =~ /\s/)) {
      return ();
    } elsif ($c eq '<' && ($next eq '<' || $next eq '(')) {
      return ();
    } elsif ($c eq '>' && $next eq '(') {
      return ();
    } elsif ($c eq '|') {
      return () if $next eq '|' || $next eq '&';
    } elsif ($c eq ';' || $c eq '&') {
      if ($c eq '&') {
        next if $prev eq '>' || $prev eq '<' || $next eq '>';
        return () unless $next eq '&';
      }
      my $len = $c eq '&' ? 2 : 1;
      return () if $next eq ';' && $c eq ';';
      my $text = substr($s, $start, $i - $start);
      $text =~ s/(\s*)\z//;
      my $before = $1;
      my $j = $i + $len;
      $j++ while $j < $n && $chars[$j] =~ /\s/;
      push @out, [$text, $before . substr($s, $i, $j - $i)];
      ($start, $i) = ($j, $j - 1);
    }
  }
  return () if $state ne '';
  my $last = substr($s, $start);
  return () if $last eq '' && (!@out || $out[-1][1] !~ /;\s*\z/);
  push @out, [$last, ''] if $last ne '';
  for my $seg (@out) {
    return () if $seg->[0] eq '';
    return () if $seg->[0] =~ /\A(?:if|then|else|elif|fi|for|while|until|do|done|case|esac|select|function|time|!)(?:\s|\z)/;
  }
  return @out;
}

# Resolves a file operand the way the shell would after `cd DIR &&`.
sub resolve_path {
  my ($file, $cwd, $cd_dir) = @_;
  my $home = $ENV{HOME} // '';
  for ($file, $cd_dir) { s{\A~(?=/|\z)}{$home} if defined }
  my $path = $file;
  if ($path !~ m{\A/}) {
    $path = "$cd_dir/$path" if defined $cd_dir;
    $path = "$cwd/$path" if $path !~ m{\A/};
  }
  return $path;
}

# The workspace root as aitools resolves it: the nearest ancestor of the working directory holding a
# `.git` entry, else the directory itself. Symlinks are resolved, as aitools compares real paths.
sub workspace_root {
  my ($cwd) = @_;
  my @parts = grep { $_ ne '' } split m{/}, $cwd;
  for (my $i = @parts; $i >= 0; $i--) {
    my $dir = '/' . join('/', @parts[0 .. $i - 1]);
    return abs_path($dir) // $dir if -e "$dir/.git";
  }
  return abs_path($cwd) // $cwd;
}

# Collapses `.` and `..` lexically, then resolves symlinks of the part that exists.
sub real_path {
  my ($path) = @_;
  my @out;
  for my $seg (split m{/}, $path) {
    next if $seg eq '' || $seg eq '.';
    if ($seg eq '..') { pop @out } else { push @out, $seg }
  }
  my @rest;
  while (@out && !-e '/' . join('/', @out)) { unshift @rest, pop @out }
  my $base = @out ? (abs_path('/' . join('/', @out)) // '/' . join('/', @out)) : '/';
  $base =~ s{/\z}{};
  return join('/', $base, @rest);
}

# True when a path operand, or the directory the command runs in, lies outside the workspace root, so
# that `aitools search` and `aitools find` would reject it. `.` stands for the implied start point and
# for the `cd DIR` target.
sub outside_root {
  my ($ctx, @paths) = @_;
  my $root = $ctx->{root} //= workspace_root($ctx->{cwd});
  for my $p (@paths, '.') {
    my $real = real_path(resolve_path($p, $ctx->{cwd}, $ctx->{cd_dir}));
    return 1 unless $real eq $root || index($real, "$root/") == 0;
  }
  return 0;
}

# True only for an existing, empty regular file.
sub is_empty_file {
  my $path = resolve_path(@_);
  return -f $path && !-s $path;
}

# True when the rewrite carries `--range A:...` and the existing file has fewer than A lines. A last
# line without a trailing newline still counts.
sub range_past_eof {
  my ($args, $cwd, $cd_dir) = @_;
  my ($i) = grep { $args->[$_] eq '--range' } 0 .. $#$args;
  return 0 unless defined $i && ($args->[$i + 1] // '') =~ /\A([0-9]+):/;
  my $start = $1;
  my $path = resolve_path($args->[1], $cwd, $cd_dir);
  return 0 unless -f $path && open(my $fh, '<:raw', $path);
  my $lines = 0;
  $lines++ while <$fh>;
  return $start > $lines;
}

# Peels the envelopes off the command. Each is matched only at the very start or end of the text: a
# `|` or `2>` inside quotes would leave an unterminated quote in the body, which split_words rejects.
sub split_envelope {
  my ($s) = @_;
  $s =~ s/\A\s+//;
  $s =~ s/\s+\z//;
  my ($redirect, $filter) = ('', undef);
  if ($s =~ s/\s*\|\s*(head|tail)(?:\s+-n\s*([0-9]+)|\s+-([0-9]+))?\z//) {
    my $n = $2 // $3 // 10;
    return ('', '', undef) unless count($n);
    $filter = [$1, $n];
  }
  if ($s =~ s{\s+(2>&1|2>/dev/null)\z}{}) {
    $redirect = " $1";
  }
  return ($s, $redirect, $filter);
}

# Folds a trailing `| head -N` or `| tail -N` into the rewrite. Only where aitools can express the
# same cut: its first N lines, results, or commits for head; the last N lines of a file for tail.
sub apply_filter {
  my ($filter, @args) = @_;
  my ($kind, $n) = @$filter;
  my %has = map { $_ => 1 } grep { /\A--/ } @args;
  my $sub = $args[0];
  if ($sub eq 'read') {
    return () if $has{'--range'} || $has{'--tail'};
    my @out = without_flag('--max-lines', @args);
    return (@out, ($kind eq 'head' ? ('--range', "1:$n") : ('--tail', $n)), '--max-lines', $n);
  }
  return () unless $kind eq 'head';
  if ($sub eq 'search') {
    # With context lines, head's N lines are fewer than N selected lines. In matches mode --limit
    # counts matches, and one line can hold several.
    return () if $has{'--before'} || $has{'--after'} || ($has{'--context'} && value_of('--context', @args) ne '0');
    return () if value_of('--output', @args) eq 'matches';
    return limit_to($n, '--limit', @args);
  }
  if ($sub eq 'find') {
    # tree prints a root line and nests its entries, so its first N lines are not N entries.
    return () if value_of('--output', @args) eq 'tree';
    return limit_to($n, '--limit', @args);
  }
  return ();
}

sub value_of {
  my ($flag, @args) = @_;
  for my $i (0 .. $#args - 1) { return $args[$i + 1] if $args[$i] eq $flag }
  return '';
}

sub without_flag {
  my ($flag, @args) = @_;
  my @out;
  while (@args) {
    my $a = shift @args;
    if ($a eq $flag) { shift @args; next }
    push @out, $a;
  }
  return @out;
}

sub limit_to {
  my ($n, $flag, @args) = @_;
  for my $i (0 .. $#args - 1) {
    if ($args[$i] eq $flag) {
      $args[$i + 1] = $n if $n < $args[$i + 1];
      return @args;
    }
  }
  return (@args, $flag, $n);
}

# Splits a command into words the way the shell would, or returns () when the shell would do more
# than split: expand, glob, redirect, or chain.
sub split_words {
  my ($s) = @_;
  $s =~ s/\A\s+//;
  $s =~ s/\s+\z//;
  return () if $s eq '';
  my @words;
  my ($word, $in_word, $state, $tilde) = ('', 0, '', 0);
  my $home = $ENV{HOME} // '';
  my @chars = split //, $s;
  my $flush = sub {
    $word = $home . substr($word, 1) if $tilde;
    push @words, $word;
    ($word, $in_word, $tilde) = ('', 0, 0);
  };
  for my $i (0 .. $#chars) {
    my $c = $chars[$i];
    if ($state eq "'") {
      if ($c eq "'") { $state = '' } else { $word .= $c }
    } elsif ($state eq '"') {
      if ($c eq '"') { $state = '' }
      elsif ($c =~ /[\$`\\!]/) { return () }
      else { $word .= $c }
    } elsif ($c =~ /\s/) {
      return () if $c eq "\n";
      $flush->() if $in_word;
    } elsif ($c eq "'" || $c eq '"') {
      ($state, $in_word) = ($c, 1);
    } elsif ($c eq '~' && !$in_word) {
      # An unquoted `~` or `~/...` is the home directory; `~user` and a tilde inside a word are not
      # taken. A quoted tilde stays literal, as in the shell, and operand() refuses it.
      return () unless $home =~ m{\A/} && ($i == $#chars || $chars[$i + 1] =~ m{[/\s]});
      ($word, $in_word, $tilde) = ('~', 1, 1);
    } elsif ($c =~ /[|&;<>()\$`\\{}!*?\[\]#~]/) {
      return ();
    } else {
      ($word, $in_word) = ($word . $c, 1);
    }
  }
  return () if $state ne '';
  $flush->() if $in_word;
  return @words;
}

sub quote {
  my ($w) = @_;
  return $w if $w =~ m{\A[A-Za-z0-9_/.,:=+\@%^-]+\z};
  $w =~ s/'/'\\''/g;
  return "'$w'";
}

sub count { return $_[0] =~ /\A[1-9][0-9]{0,6}\z/ }

sub rewrite {
  my ($ctx, $cmd, @args) = @_;
  my %table = (
    # read, info, json get, and diff take any path, so these need no root check.
    cat => \&rw_cat, head => \&rw_head, tail => \&rw_tail, sed => \&rw_sed, diff => \&rw_diff,
    jq => \&rw_jq, wc => sub { rw_wc($ctx, @_) },
    # search and find only take paths inside the workspace root.
    grep => sub { rw_grep($ctx, @_) }, ls => sub { rw_ls($ctx, @_) }, find => sub { rw_find($ctx, @_) },
    # These also add the hidden-entry globs, whose fit depends on the `cd DIR &&` envelope.
    rg => sub { rw_rg($ctx, @_) }, fd => sub { rw_fd($ctx, @_) }, tree => sub { rw_tree($ctx, @_) },
  );
  my $handler = $table{$cmd} or return ();
  return $handler->(@args);
}

# A path operand must not look like an option, so a stray flag is never passed on as a file name. A
# `~`-prefixed word is refused: split_words already expanded an unquoted tilde to the home directory,
# so what remains was quoted, and a quoted tilde is a literal name that the writer likely did not mean.
sub operand { return defined $_[0] && $_[0] ne '' && $_[0] !~ /\A[-~]/ }

sub rw_cat {
  my @args = grep { $_ ne '-n' } @_;
  return () unless @args == 1 && operand($args[0]);
  return ('read', $args[0], '--max-lines', '2000');
}

# head FILE, head -n N FILE, head -nN FILE, head -N FILE
sub line_count_and_file {
  my @args = @_;
  my $n = 10;
  if (@args && $args[0] eq '-n') {
    shift @args;
    $n = shift @args;
  } elsif (@args && $args[0] =~ /\A-n?([0-9]+)\z/) {
    $n = $1;
    shift @args;
  }
  return () unless defined $n && count($n) && @args == 1 && operand($args[0]);
  return ($n, $args[0]);
}

sub rw_head {
  my ($n, $file) = line_count_and_file(@_) or return ();
  return ('read', $file, '--range', "1:$n", '--max-lines', $n);
}

sub rw_tail {
  my ($n, $file) = line_count_and_file(@_) or return ();
  return ('read', $file, '--tail', $n, '--max-lines', $n);
}

# Only the print-a-line-range form: sed -n 'Ap', 'A,Bp', 'A,$p'.
sub rw_sed {
  my @args = @_;
  return () unless @args == 3 && $args[0] eq '-n' && operand($args[2]);
  my ($a, $b) = $args[1] =~ /\A([1-9][0-9]*)(?:,([1-9][0-9]*|\$))?p\z/ or return ();
  $b //= $a;
  if ($b eq '$') {
    return ('read', $args[2], '--range', "$a:", '--max-lines', '2000');
  }
  return () if $b < $a;
  return ('read', $args[2], '--range', "$a:$b", '--max-lines', $b - $a + 1);
}

# grep's default dialect is BRE, where + ? | ( ) { } are literal and \ introduces the operators;
# aitools reads the same text as a modern regex, so any of those characters changes the meaning.
sub bre_safe { return $_[0] !~ /[\\+?|(){}]/ }

# Rewrites a GNU BRE into the modern regex dialect aitools reads: \| \( \) \{ \} \+ \? become the bare
# operators, and the bare characters, literal in a BRE, make the pattern stay as typed. Escaped
# punctuation and bracket expressions carry over; escaped letters and digits (\w, \1, ...) differ
# between grep implementations, and a backslash inside a bracket expression is a literal there but an
# escape for aitools, so they stay as typed too.
sub bre_translate {
  my ($p) = @_;
  return undef if $p =~ /\A\*/;
  my @c = split //, $p;
  my $out = '';
  # nonempty: the current alternation branch holds something. quantifiable: the last item is an atom
  # a quantifier may follow (not the start of a branch, an anchor, or another quantifier).
  my ($nonempty, $quantifiable) = (0, 0);
  my $quantify = sub {
    return 0 unless $quantifiable;
    $quantifiable = 0;
    return 1;
  };
  while (@c) {
    my $ch = shift @c;
    if ($ch eq '\\') {
      my $e = shift @c;
      return undef unless defined $e;
      if ($e eq '(' || $e eq '|') {
        return undef if $e eq '|' && !$nonempty;
        $out .= $e;
        ($nonempty, $quantifiable) = (0, 0);
      } elsif ($e eq ')') {
        return undef unless $nonempty;
        $out .= $e;
        ($nonempty, $quantifiable) = (1, 1);
      } elsif ($e eq '{') {
        # GNU reads \{,N\} as 0 to N; the modern dialect rejects it.
        return undef if @c && $c[0] eq ',';
        return undef unless $quantify->();
        $out .= $e;
      } elsif ($e eq '}') {
        $out .= $e;
        ($nonempty, $quantifiable) = (1, 0);
      } elsif ($e eq '+' || $e eq '?') {
        return undef unless $quantify->();
        $out .= $e;
      } elsif ($e =~ /[A-Za-z0-9<>`']/) {
        return undef;
      } else {
        $out .= "\\$e";
        ($nonempty, $quantifiable) = (1, 1);
      }
    } elsif ($ch eq '[') {
      $out .= $ch;
      for my $lead ('^', ']') { $out .= shift @c if @c && $c[0] eq $lead }
      my $closed = 0;
      while (@c) {
        my $b = shift @c;
        return undef if $b eq '\\';
        $out .= $b;
        if ($b eq '[' && @c && $c[0] =~ /[:.=]/) {
          my $kind = shift @c;
          $out .= $kind;
          while (@c) {
            my $d = shift @c;
            $out .= $d;
            if ($d eq $kind && @c && $c[0] eq ']') { $out .= shift @c; last }
          }
        } elsif ($b eq ']') { $closed = 1; last }
      }
      return undef unless $closed;
      ($nonempty, $quantifiable) = (1, 1);
    } elsif ($ch =~ /[+?|(){}]/) {
      return undef;
    } elsif ($ch eq '*') {
      # At the start of a branch, after an anchor, or after a quantifier a BRE star is literal or
      # redundant, and the modern dialect reads it differently.
      return undef unless $quantify->();
      $out .= $ch;
    } elsif ($ch eq '^') {
      # An anchor only at the start of the pattern or of a group or branch; elsewhere a literal in a BRE.
      return undef if $nonempty;
      $out .= $ch;
      ($nonempty, $quantifiable) = (1, 0);
    } elsif ($ch eq '$') {
      # An anchor only at the end of the pattern or before \) or \|; elsewhere a literal in a BRE.
      my $closes = !@c || ($c[0] eq '\\' && @c > 1 && $c[1] =~ /[)|]/);
      return undef unless $closes;
      $out .= $ch;
      ($nonempty, $quantifiable) = (1, 0);
    } else {
      $out .= $ch;
      ($nonempty, $quantifiable) = (1, 1);
    }
  }
  return undef unless $nonempty;
  return $out;
}

# ERE and rg syntax agree with aitools apart from backslash escapes: keep the ones all three read
# alike (character-class shorthands, word boundaries, escaped punctuation). GNU's \< \> \` \' are
# anchors there but literal characters in aitools. A brace outside a {n}, {n,}, or {n,m} quantifier
# is a literal to grep -E and rg but a syntax error to aitools.
sub escapes_safe {
  my ($p) = @_;
  while ($p =~ /\\(.)/g) {
    return 0 unless $1 =~ /[bBdDsSwW]|[^A-Za-z0-9<>`']/;
  }
  (my $bare = $p) =~ s/\\.//g;
  $bare =~ s/\{[0-9]+(?:,[0-9]*)?\}//g;
  return $bare !~ /[{}]/;
}

# Parses grep/rg arguments against a table of accepted flags. Each table entry is either a string
# (a switch that maps to that aitools flag, or '' to drop it) or a code ref taking a value.
sub search_args {
  my ($short, $long, @args) = @_;
  my (%opt, @patterns, @paths);
  my $dashdash = 0;
  while (@args) {
    my $a = shift @args;
    if ($dashdash || $a !~ /\A-/ || $a eq '-') {
      return () if $a eq '-';
      if (!@patterns && !$opt{explicit_pattern}) { push @patterns, $a } else { push @paths, $a }
      next;
    }
    if ($a eq '--') { $dashdash = 1; next }
    if ($a =~ /\A--([a-z-]+)(?:=(.*))?\z/) {
      my ($name, $value) = ($1, $2);
      my $spec = $long->{$name} // return ();
      if (ref $spec) {
        $value //= shift @args;
        return () unless defined $value;
        $spec->(\%opt, $value, \@patterns) or return ();
      } else {
        return () if defined $value;
        $opt{$spec} = 1 if $spec ne '';
      }
      next;
    }
    my @letters = split //, substr($a, 1);
    while (defined(my $l = shift @letters)) {
      my $spec = $short->{$l} // return ();
      if (ref $spec) {
        my $value = @letters ? join('', @letters) : shift @args;
        @letters = ();
        return () unless defined $value;
        $spec->(\%opt, $value, \@patterns) or return ();
      } else {
        $opt{$spec} = 1 if $spec ne '';
      }
    }
  }
  return () unless @patterns;
  # A pattern or path that starts with `-` would reach aitools as one of its own options.
  return () if grep { !operand($_) } @patterns, @paths;
  return (\%opt, \@patterns, \@paths);
}

sub context_setter {
  my ($key) = @_;
  return sub { my ($opt, $v) = @_; return 0 unless $v =~ /\A[0-9]{1,4}\z/; $opt->{$key} = $v; 1 };
}

sub pattern_setter {
  return sub { my ($opt, $v, $patterns) = @_; push @$patterns, $v; $opt->{explicit_pattern} = 1; 1 };
}

sub glob_setter {
  return sub { my ($opt, $v) = @_; return 0 if defined $opt->{glob} || !operand($v); $opt->{glob} = $v; 1 };
}

# rg's type names whose file sets equal an aitools language's extensions (`rg --type-list`).
sub rg_lang {
  my %lang = (
    nix => 'nix', rust => 'rust', go => 'go', py => 'python', python => 'python',
    ts => 'typescript', typescript => 'typescript',
  );
  return $lang{ $_[0] // '' };
}

sub type_setter {
  return sub { my ($opt, $v) = @_; return 0 if defined $opt->{lang}; $opt->{lang} = rg_lang($v) // return 0; 1 };
}

# Flag combinations whose aitools meaning differs from grep or rg, or is unverified, stay as typed.
sub combination_ok {
  my ($opt, $patterns) = @_;
  my $context = grep { defined $opt->{$_} } qw(context before after);
  return 0 if $opt->{files} && $opt->{count};
  return 0 if $opt->{only} && ($opt->{invert} || $opt->{files} || $opt->{count} || $context);
  return 0 if $opt->{invert} && ($opt->{files} || $opt->{count} || $opt->{multiline} || @$patterns > 1);
  return 0 if $opt->{line} && ($opt->{word} || @$patterns > 1);
  return 1;
}

# rg's -g matches directories as well as files, and a glob that selects files also brings back
# gitignored files and dot-files it names; aitools globs only filter files. An excluded extension
# reads the same in both.
sub rg_glob_ok { return $_[0] =~ /\A!\*\.[A-Za-z0-9_.]+\z/ }

sub rg_glob_setter {
  return sub { my ($opt, $v) = @_; return 0 if defined $opt->{glob} || !rg_glob_ok($v); $opt->{glob} = $v; 1 };
}

# rg, fd, and tree skip dot-files and the contents of dot-directories; aitools lists both. rg -t
# still lists the dot-files of its type, so it keeps only the dot-directory glob.
sub hidden_globs {
  my ($keep_dot_files) = @_;
  return (($keep_dot_files ? () : ('--glob', '!.*')), '--glob', '!**/.*/**');
}

# aitools takes its workspace from the nearest directory holding a `.git` entry, as its root
# resolution does, and matches globs against paths relative to it. Below a dot-directory of that
# workspace, the dot-directory glob would exclude every result.
sub under_dot_directory {
  my ($cwd) = @_;
  return 1 unless defined $cwd && $cwd =~ m{\A/};
  my @parts = grep { $_ ne '' } split m{/}, $cwd;
  for (my $i = @parts; $i >= 0; $i--) {
    next unless -e '/' . join('/', @parts[0 .. $i - 1], '.git');
    return scalar grep { /\A\./ } @parts[$i .. $#parts];
  }
  return 0;
}

# Those globs match workspace-relative paths, so a start point that is hidden itself, or that the
# hook cannot place in that frame (absolute, home-relative, or `..`), would lose everything under it.
sub hidden_globs_fit {
  my ($ctx, @paths) = @_;
  return 0 if $ctx->{hidden_cwd};
  for my $p (grep { defined } $ctx->{cd_dir}, @paths) {
    return 0 if $p =~ m{\A[/~]} || grep { /\A\./ && $_ ne '.' } split m{/}, $p;
  }
  return 1;
}

sub search_command {
  my ($opt, $patterns, $paths) = @_;
  my @out = ('search');
  if (@$patterns == 1) {
    push @out, $patterns->[0];
  } else {
    push @out, map { ('--pattern', $_) } @$patterns;
  }
  push @out, @$paths;
  push @out, '--fixed' if $opt->{fixed};
  push @out, '--ignore-case' if $opt->{icase};
  push @out, '--word' if $opt->{word};
  push @out, '--line-regexp' if $opt->{line};
  push @out, '--invert' if $opt->{invert};
  push @out, '--multiline' if $opt->{multiline};
  if ($opt->{files}) {
    push @out, '--output', 'files', '--limit', '200';
  } elsif ($opt->{count}) {
    push @out, '--output', 'count', '--limit', '200';
  } elsif ($opt->{only}) {
    push @out, '--output', 'matches', '--limit', '100';
  } else {
    push @out, '--context', $opt->{context} // 0;
    push @out, '--before', $opt->{before} if defined $opt->{before};
    push @out, '--after', $opt->{after} if defined $opt->{after};
    push @out, '--limit', '100';
  }
  push @out, '--glob', $opt->{glob} if defined $opt->{glob};
  push @out, '--lang', $opt->{lang} if defined $opt->{lang};
  push @out, hidden_globs(defined $opt->{lang}) if $opt->{hidden_globs};
  push @out, '--no-ignore' if $opt->{no_ignore};
  return @out;
}

sub rw_grep {
  my ($ctx, @args) = @_;
  my %short = (
    n => '', H => '', s => '', I => '', r => 'recursive', R => 'recursive',
    i => 'icase', w => 'word', F => 'fixed', E => 'extended', l => 'files', c => 'count',
    o => 'only', v => 'invert', x => 'line',
    e => pattern_setter(), A => context_setter('after'), B => context_setter('before'),
    C => context_setter('context'),
  );
  my %long = (
    'line-number' => '', recursive => 'recursive', 'ignore-case' => 'icase',
    'word-regexp' => 'word', 'fixed-strings' => 'fixed', 'extended-regexp' => 'extended',
    'files-with-matches' => 'files', count => 'count', regexp => pattern_setter(),
    'only-matching' => 'only', 'invert-match' => 'invert', 'line-regexp' => 'line',
    include => glob_setter(),
  );
  my ($opt, $patterns, $paths) = search_args(\%short, \%long, @args) or return ();
  return () if outside_root($ctx, @$paths);
  # Without a path grep reads stdin, which a hook cannot supply.
  return () if !@$paths && !$opt->{recursive};
  return () unless combination_ok($opt, $patterns);
  unless ($opt->{fixed}) {
    for my $p (@$patterns) {
      if ($opt->{extended}) {
        return () unless escapes_safe($p);
      } else {
        $p = bre_translate($p) // return ();
      }
    }
  }
  # grep reads every file it is pointed at; aitools, like rg, skips .gitignore'd ones by default.
  $opt->{no_ignore} = 1;
  return search_command($opt, $patterns, $paths);
}

# rg --hidden is not taken: it also searches .git, which aitools always skips.
sub rw_rg {
  my ($ctx, @args) = @_;
  return rw_rg_files($ctx, @args) if grep { $_ eq '--files' } @args;
  my %short = (
    n => '', N => '', i => 'icase', w => 'word', F => 'fixed', l => 'files', c => 'count',
    o => 'only', v => 'invert', x => 'line', U => 'multiline', t => type_setter(),
    e => pattern_setter(), A => context_setter('after'), B => context_setter('before'),
    C => context_setter('context'),
    g => rg_glob_setter(),
  );
  my %long = (
    'line-number' => '', 'no-heading' => '', 'no-config' => '', 'ignore-case' => 'icase',
    'word-regexp' => 'word', 'fixed-strings' => 'fixed', 'files-with-matches' => 'files', count => 'count',
    'only-matching' => 'only', 'invert-match' => 'invert', 'line-regexp' => 'line',
    multiline => 'multiline', type => $short{t},
    regexp => pattern_setter(), glob => $short{g},
  );
  my ($opt, $patterns, $paths) = search_args(\%short, \%long, @args) or return ();
  return () unless combination_ok($opt, $patterns) && hidden_globs_fit($ctx, @$paths);
  return () if outside_root($ctx, @$paths);
  unless ($opt->{fixed}) {
    for my $p (@$patterns) { return () unless escapes_safe($p) }
  }
  $opt->{hidden_globs} = 1;
  return search_command($opt, $patterns, $paths);
}

# rg --files [-g GLOB] [-t TYPE] [PATH]: the files rg would search.
sub rw_rg_files {
  my ($ctx, @args) = @_;
  my %takes = ('-g' => 'glob', '--glob' => 'glob', '-t' => 'type', '--type' => 'type');
  my (%opt, @paths);
  while (@args) {
    my $a = shift @args;
    next if $a eq '--files';
    if (my $key = $takes{$a}) {
      my $v = shift @args;
      return () unless operand($v) && !defined $opt{$key};
      $opt{$key} = $v;
      next;
    }
    return () unless operand($a);
    push @paths, $a;
  }
  return () if @paths > 1 || !hidden_globs_fit($ctx, @paths) || outside_root($ctx, @paths);
  return () if defined $opt{glob} && !rg_glob_ok($opt{glob});
  my $lang;
  if (defined $opt{type}) { $lang = rg_lang($opt{type}) // return () }
  return (
    'find', '*', @paths, '--type', 'file', (defined $lang ? ('--lang', $lang) : ()),
    (defined $opt{glob} ? ('--glob', $opt{glob}) : ()), hidden_globs(defined $lang), '--limit', '200',
  );
}

# fd [PATTERN [PATH]]. fd reads PATTERN as a regex over the name and is smart-case, while an aitools
# pattern is a case-sensitive substring, so only patterns both read alike pass: none, `.`, or a
# literal holding an uppercase letter. fd skips .git under -H, as aitools always does, but lists it
# under -H -I, so that pair stays as typed.
sub rw_fd {
  my ($ctx, @args) = @_;
  my %types = (f => 'file', file => 'file', d => 'dir', dir => 'dir', directory => 'dir');
  my (%opt, @operands);
  while (@args) {
    my $a = shift @args;
    if ($a eq '-H' || $a eq '--hidden') { $opt{hidden} = 1 }
    elsif ($a eq '-I' || $a eq '--no-ignore') { $opt{no_ignore} = 1 }
    elsif ($a =~ /\A(?:-t|--type=?)(.*)\z/) {
      my $v = $1 ne '' ? $1 : shift @args;
      return () if defined $opt{type} || !defined $v;
      $opt{type} = $types{$v} // return ();
    } elsif ($a eq '-d' || $a eq '--max-depth') {
      my $v = shift @args;
      return () if defined $opt{depth} || !defined $v || !count($v);
      $opt{depth} = $v;
    } elsif (operand($a)) {
      push @operands, $a;
    } else {
      return ();
    }
  }
  return () if @operands > 2 || ($opt{hidden} && $opt{no_ignore});
  my ($pattern, @path) = @operands;
  if (!defined $pattern || $pattern eq '.') {
    $pattern = '*';
  } else {
    return () unless $pattern =~ /\A[A-Za-z0-9_-]+\z/ && $pattern =~ /[A-Z]/;
  }
  return () unless $opt{hidden} || hidden_globs_fit($ctx, @path);
  return () if outside_root($ctx, @path);
  return (
    'find', $pattern, @path, (defined $opt{type} ? ('--type', $opt{type}) : ()),
    (defined $opt{depth} ? ('--depth', $opt{depth}) : ()), ($opt{no_ignore} ? ('--no-ignore') : ()),
    ($opt{hidden} ? () : hidden_globs()), '--limit', '200',
  );
}

# tree [-d] [-L N] [PATH]. tree reads no .gitignore and hides dot entries; tree -a would also list
# .git, which aitools never does, so it stays as typed.
sub rw_tree {
  my ($ctx, @args) = @_;
  my ($dirs, $depth, @paths);
  while (@args) {
    my $a = shift @args;
    if ($a eq '-d') {
      $dirs = 1;
    } elsif ($a eq '-L') {
      $depth = shift @args;
      return () unless defined $depth && count($depth);
    } elsif (operand($a)) {
      push @paths, $a;
    } else {
      return ();
    }
  }
  return () if @paths > 1 || !hidden_globs_fit($ctx, @paths) || outside_root($ctx, @paths);
  return (
    'find', '*', @paths, '--output', 'tree', ($dirs ? ('--type', 'dir') : ()),
    (defined $depth ? ('--depth', $depth) : ()), '--no-ignore', hidden_globs(), '--limit', '200',
  );
}

# wc -c FILE: info's size is the byte count. wc -l FILE: info's lines field also counts an
# unterminated last line, so only an existing file that is empty or ends in a newline is taken.
sub rw_wc {
  my ($ctx, @args) = @_;
  return () unless @args == 2 && operand($args[1]);
  return ('info', $args[1]) if $args[0] eq '-c';
  return () unless $args[0] eq '-l';
  my $path = resolve_path($args[1], $ctx->{cwd}, $ctx->{cd_dir});
  return () unless -f $path && open(my $fh, '<:raw', $path);
  return ('info', $args[1]) unless -s $path;
  seek($fh, -1, 2);
  read($fh, my $last, 1);
  return () unless defined $last && $last eq "\n";
  return ('info', $args[1]);
}

# jq [-r] FILTER FILE, where FILTER is `.` or a chain of `.name` and `[N]` steps that map one to one
# onto JSON pointer segments.
sub rw_jq {
  my @args = @_;
  my $raw = 0;
  while (@args && $args[0] eq '-r') { $raw = 1; shift @args }
  return () unless @args == 2 && operand($args[1]);
  my ($filter, $file) = @args;
  my $name = qr/[A-Za-z_][A-Za-z0-9_]*/;
  my $index = qr/\[(?:0|[1-9][0-9]*)\]/;
  my $pointer = '';
  if ($filter ne '.') {
    return () unless $filter =~ /\A(?:\.$name|\.$index)(?:\.$name|\.?$index)*\z/;
    $pointer = join '', map { "/$_" } $filter =~ /([A-Za-z_][A-Za-z0-9_]*|[0-9]+)/g;
  }
  return ('json', 'get', $file, $pointer, ($raw ? ('--raw') : ()));
}

sub rw_ls {
  my ($ctx, @args) = @_;
  my (@paths, $all);
  for my $a (@args) {
    if ($a =~ /\A-[laAh1]+\z/) { $all = 1 if $a =~ /[aA]/; next }
    return () unless operand($a);
    push @paths, $a;
  }
  return () if @paths > 1 || outside_root($ctx, @paths);
  # ls hides dot entries unless -a or -A; aitools find lists them. At depth 1 the dot-file glob is
  # enough, and the dot-directory glob would empty `ls .config`.
  return ('find', '*', @paths, '--depth', '1', '--no-ignore', ($all ? () : ('--glob', '!.*')), '--limit', '200');
}

# find [PATH] with any of -maxdepth N, -type f|d, -name GLOB. find -name matches a whole name while
# an aitools pattern without a glob character is a substring match, so -name must carry a glob.
sub rw_find {
  my ($ctx, @args) = @_;
  my $path = (@args && operand($args[0])) ? shift @args : undef;
  return () if outside_root($ctx, grep { defined } $path);
  my ($depth, $type, $name);
  while (@args) {
    my $a = shift @args;
    my $v = shift @args;
    return () unless defined $v;
    if ($a eq '-maxdepth' && !defined $depth && count($v)) { $depth = $v }
    elsif ($a eq '-type' && !defined $type && $v =~ /\A[fd]\z/) { $type = $v eq 'f' ? 'file' : 'dir' }
    elsif ($a eq '-name' && !defined $name && operand($v) && $v =~ /[*?\[]/ && $v !~ m{/}) { $name = $v }
    else { return () }
  }
  my @out = ('find', $name // '*');
  push @out, $path if defined $path;
  push @out, '--type', $type if defined $type;
  push @out, '--depth', $depth if defined $depth;
  push @out, '--no-ignore', '--limit', '200';
  return @out;
}

sub rw_diff {
  my @args = grep { $_ ne '-u' } @_;
  return () unless @args == 2 && operand($args[0]) && operand($args[1]);
  return ('diff', @args);
}
PERL

perl -e "$perl_prog" "$mode" "$input" 2>/dev/null || true
