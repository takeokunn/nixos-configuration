#!/bin/bash
# PreToolUse:Bash hook that rewrites a read-only shell command (cat, head, tail, sed -n, grep, rg,
# ls, find, fd, tree, wc -c, jq, diff, and the reading git sub-commands) into the equivalent
# `aitools` call, so the model gets aitools' JSON with line numbers, hashes, and next_commands
# instead of raw text.
#
# The first argument selects the output shape:
#   claude (default)  hookSpecificOutput.updatedInput only. A permissionDecision would auto-approve
#                     the command sight-unseen.
#   codex             Codex rejects updatedInput unless permissionDecision is "allow". The rewrite
#                     only ever yields a reading aitools command, so nothing mutating is approved.
#   plain             stdin is the raw command; stdout is the rewrite, or empty for no rewrite. Used
#                     by the opencode plugin.
#
# The command must be one simple command, optionally wrapped in the three envelopes agents add most:
# a leading `cd DIR &&`, a trailing `2>&1` or `2>/dev/null`, and a trailing `| head -N` or
# `| tail -N`, which becomes aitools' own line or result limit where aitools can count the same
# units (not for tree output, rg -o, or a search with context lines). Any other pipeline, list,
# substitution, or redirect feeds the output to another program, and aitools' JSON would break it.
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
# jq is rewritten only for `.` or a chain of `.name` and `[N]` steps. Its result differs on one
# input: jq prints null for a missing key, while `aitools json get` fails with input.not-found and
# lists the pointers that do exist.

set -euo pipefail

mode="${1:-claude}"
input=$(cat)

if ! command -v aitools &>/dev/null || ! command -v perl &>/dev/null; then
  exit 0
fi

# Bound to a variable, not heredoc-fed via $(...), since macOS's bash 3.2 mis-parses a heredoc
# containing a backtick inside $(...).
read -r -d '' perl_prog <<'PERL' || true
use strict;
use warnings;
use JSON::PP;
use Cwd qw(getcwd);

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

my ($prefix, $body, $redirect, $filter, $cd_dir) = split_envelope($command);
my @words = split_words($body) or exit 0;
my %ctx = (cd_dir => $cd_dir, hidden_cwd => under_dot_directory($cwd));
my @rewritten = rewrite(\%ctx, @words) or exit 0;
if ($filter) {
  @rewritten = apply_filter($filter, \@words, @rewritten) or exit 0;
}
my $new = $prefix . join(' ', map { quote($_) } 'aitools', @rewritten) . $redirect;

if ($mode eq 'plain') {
  print $new;
  exit 0;
}
my %updated = (%$tool_input, command => $new);
my %out = (hookEventName => 'PreToolUse', updatedInput => \%updated);
$out{permissionDecision} = 'allow' if $mode eq 'codex';
print JSON::PP->new->canonical->encode({ hookSpecificOutput => \%out });
exit 0;

# Peels the envelopes off the command. Each is matched only at the very start or end of the text: a
# `|` or `2>` inside quotes would leave an unterminated quote in the body, which split_words rejects.
sub split_envelope {
  my ($s) = @_;
  $s =~ s/\A\s+//;
  $s =~ s/\s+\z//;
  my ($prefix, $redirect, $filter, $cd_dir) = ('', '', undef, undef);
  if ($s =~ s{\A(cd\s+([A-Za-z0-9_/.,:\@%+~-]+)\s*&&\s*)}{}) {
    ($prefix, $cd_dir) = ($1, $2);
  }
  if ($s =~ s/\s*\|\s*(head|tail)(?:\s+-n\s*([0-9]+)|\s+-([0-9]+))?\z//) {
    my $n = $2 // $3 // 10;
    return ('', '', '', undef) unless count($n);
    $filter = [$1, $n];
  }
  if ($s =~ s{\s+(2>&1|2>/dev/null)\z}{}) {
    $redirect = " $1";
  }
  return ($prefix, $s, $redirect, $filter, $cd_dir);
}

# Folds a trailing `| head -N` or `| tail -N` into the rewrite. Only where aitools can express the
# same cut: its first N lines, results, or commits for head; the last N lines of a file for tail.
sub apply_filter {
  my ($filter, $words, @args) = @_;
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
  if ($sub eq 'git') {
    # Without --oneline a commit spans several lines, so head's N lines are fewer than N commits.
    return limit_to($n, '--limit', @args) if $args[1] eq 'log' && grep { $_ eq '--oneline' } @$words;
    return limit_to($n, '--max-lines', @args) if $args[1] eq 'show';
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
  my ($word, $in_word, $state) = ('', 0, '');
  for my $c (split //, $s) {
    if ($state eq "'") {
      if ($c eq "'") { $state = '' } else { $word .= $c }
    } elsif ($state eq '"') {
      if ($c eq '"') { $state = '' }
      elsif ($c =~ /[\$`\\!]/) { return () }
      else { $word .= $c }
    } elsif ($c =~ /\s/) {
      return () if $c eq "\n";
      if ($in_word) { push @words, $word; ($word, $in_word) = ('', 0) }
    } elsif ($c eq "'" || $c eq '"') {
      ($state, $in_word) = ($c, 1);
    } elsif ($c =~ /[|&;<>()\$`\\{}!*?\[\]#~]/) {
      return ();
    } else {
      ($word, $in_word) = ($word . $c, 1);
    }
  }
  return () if $state ne '';
  push @words, $word if $in_word;
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
    cat => \&rw_cat, head => \&rw_head, tail => \&rw_tail, sed => \&rw_sed,
    grep => \&rw_grep, ls => \&rw_ls, find => \&rw_find, diff => \&rw_diff,
    git => \&rw_git, wc => \&rw_wc, jq => \&rw_jq,
    # These add the hidden-entry globs, whose fit depends on the `cd DIR &&` envelope.
    rg => sub { rw_rg($ctx, @_) }, fd => sub { rw_fd($ctx, @_) }, tree => sub { rw_tree($ctx, @_) },
  );
  my $handler = $table{$cmd} or return ();
  return $handler->(@args);
}

# A path operand must not look like an option, so a stray flag is never passed on as a file name.
sub operand { return defined $_[0] && $_[0] ne '' && $_[0] !~ /\A-/ }

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
  my ($opt, $patterns, $paths) = search_args(\%short, \%long, @_) or return ();
  # Without a path grep reads stdin, which a hook cannot supply.
  return () if !@$paths && !$opt->{recursive};
  return () unless combination_ok($opt, $patterns);
  unless ($opt->{fixed}) {
    for my $p (@$patterns) {
      return () unless $opt->{extended} ? escapes_safe($p) : bre_safe($p);
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
    'line-number' => '', 'no-heading' => '', 'ignore-case' => 'icase', 'word-regexp' => 'word',
    'fixed-strings' => 'fixed', 'files-with-matches' => 'files', count => 'count',
    'only-matching' => 'only', 'invert-match' => 'invert', 'line-regexp' => 'line',
    multiline => 'multiline', type => $short{t},
    regexp => pattern_setter(), glob => $short{g},
  );
  my ($opt, $patterns, $paths) = search_args(\%short, \%long, @args) or return ();
  return () unless combination_ok($opt, $patterns) && hidden_globs_fit($ctx, @$paths);
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
  return () if @paths > 1 || !hidden_globs_fit($ctx, @paths);
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
  return () if @paths > 1 || !hidden_globs_fit($ctx, @paths);
  return (
    'find', '*', @paths, '--output', 'tree', ($dirs ? ('--type', 'dir') : ()),
    (defined $depth ? ('--depth', $depth) : ()), '--no-ignore', hidden_globs(), '--limit', '200',
  );
}

# wc -c FILE only: info's size is the byte count. wc -l stays as typed because info's lines field
# also counts an unterminated last line.
sub rw_wc {
  my @args = @_;
  return () unless @args == 2 && $args[0] eq '-c' && operand($args[1]);
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
  my @args = @_;
  my (@paths, $all);
  for my $a (@args) {
    if ($a =~ /\A-[laAh1]+\z/) { $all = 1 if $a =~ /[aA]/; next }
    return () unless operand($a);
    push @paths, $a;
  }
  return () if @paths > 1;
  # ls hides dot entries unless -a or -A; aitools find lists them. At depth 1 the dot-file glob is
  # enough, and the dot-directory glob would empty `ls .config`.
  return ('find', '*', @paths, '--depth', '1', '--no-ignore', ($all ? () : ('--glob', '!.*')), '--limit', '200');
}

# find [PATH] with any of -maxdepth N, -type f|d, -name GLOB. find -name matches a whole name while
# an aitools pattern without a glob character is a substring match, so -name must carry a glob.
sub rw_find {
  my @args = @_;
  my $path = (@args && operand($args[0])) ? shift @args : undef;
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

sub rw_git {
  my ($sub, @args) = @_;
  return () unless defined $sub;
  if ($sub eq 'status') {
    return () if grep { !/\A(?:-s|--short|-sb|-b|--branch)\z/ } @args;
    return ('git', 'status');
  }
  if ($sub eq 'log') {
    my ($limit, $path) = (20, undef);
    while (@args) {
      my $a = shift @args;
      if ($a eq '--oneline') { next }
      elsif ($a eq '-n') { $limit = shift @args; return () unless defined $limit && count($limit) }
      elsif ($a =~ /\A-n?([0-9]+)\z/) { $limit = $1; return () unless count($limit) }
      elsif ($a eq '--') { $path = shift @args; return () unless operand($path) && !@args }
      else { return () }
    }
    return ('git', 'log', (defined $path ? ($path) : ()), '--limit', $limit);
  }
  if ($sub eq 'diff') {
    my (@flags, $path);
    while (@args) {
      my $a = shift @args;
      if ($a eq '--staged' || $a eq '--cached') { push @flags, '--staged' }
      elsif ($a eq '--stat') { push @flags, '--output', 'stat' }
      elsif ($a eq '--') { $path = shift @args; return () unless operand($path) && !@args }
      else { return () }
    }
    return ('git', 'diff', (defined $path ? ($path) : ()), @flags);
  }
  if ($sub eq 'show') {
    return () unless @args == 1 && $args[0] =~ /\A[^-:\s][^:\s]*:[^:\s]+\z/;
    return ('git', 'show', $args[0], '--max-lines', '2000');
  }
  if ($sub eq 'blame') {
    my ($range, @paths);
    while (@args) {
      my $a = shift @args;
      if ($a eq '-L') {
        my $v = shift @args // return ();
        my ($s, $e) = split /,/, $v, 2;
        return () unless defined $e && count($s) && count($e) && $e >= $s;
        $range = "$s:$e";
      } elsif (operand($a)) { push @paths, $a }
      else { return () }
    }
    return () unless @paths == 1;
    return ('git', 'blame', $paths[0], (defined $range ? ('--range', $range) : ()));
  }
  return ();
}
PERL

perl -e "$perl_prog" "$mode" "$input" 2>/dev/null || true
