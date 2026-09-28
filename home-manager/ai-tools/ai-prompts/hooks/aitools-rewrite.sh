#!/bin/bash
# PreToolUse:Bash hook that rewrites a read-only shell command (cat, head, tail, sed -n, grep, rg,
# ls, find, diff, and the reading git sub-commands) into the equivalent `aitools` call, so the
# model gets aitools' JSON with line numbers, hashes, and next_commands instead of raw text.
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
# `| tail -N`, which becomes aitools' own line or result limit. Any other pipeline, list,
# substitution, or redirect feeds the output to another program, and aitools' JSON would break it.
# A flag outside the per-command allowlist, an unquoted glob, a variable, or a regex whose dialect
# differs from aitools' leaves the command untouched. Everything fails open. To keep a command as
# typed, prefix it: `command cat f`.

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

my ($mode, $input) = @ARGV;

my ($command, $tool_input);
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
}
exit 0 unless defined $command && !ref $command;

my ($prefix, $body, $redirect, $filter) = split_envelope($command);
my @words = split_words($body) or exit 0;
my @rewritten = rewrite(@words) or exit 0;
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
  my ($prefix, $redirect, $filter) = ('', '', undef);
  if ($s =~ s{\A(cd\s+[A-Za-z0-9_/.,:\@%+~-]+\s*&&\s*)}{}) {
    $prefix = $1;
  }
  if ($s =~ s/\s*\|\s*(head|tail)(?:\s+-n\s*([0-9]+)|\s+-([0-9]+))?\z//) {
    my $n = $2 // $3 // 10;
    return ('', '', '', undef) unless count($n);
    $filter = [$1, $n];
  }
  if ($s =~ s{\s+(2>&1|2>/dev/null)\z}{}) {
    $redirect = " $1";
  }
  return ($prefix, $s, $redirect, $filter);
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
    # With context lines, head's N lines are fewer than N selected lines.
    return () if $has{'--before'} || $has{'--after'} || ($has{'--context'} && context_of(@args) ne '0');
    return limit_to($n, '--limit', @args);
  }
  return limit_to($n, '--limit', @args) if $sub eq 'find';
  if ($sub eq 'git') {
    # Without --oneline a commit spans several lines, so head's N lines are fewer than N commits.
    return limit_to($n, '--limit', @args) if $args[1] eq 'log' && grep { $_ eq '--oneline' } @$words;
    return limit_to($n, '--max-lines', @args) if $args[1] eq 'show';
  }
  return ();
}

sub context_of {
  my @args = @_;
  for my $i (0 .. $#args - 1) { return $args[$i + 1] if $args[$i] eq '--context' }
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
  my ($cmd, @args) = @_;
  my %table = (
    cat => \&rw_cat, head => \&rw_head, tail => \&rw_tail, sed => \&rw_sed,
    grep => \&rw_grep, rg => \&rw_rg, ls => \&rw_ls, find => \&rw_find, diff => \&rw_diff,
    git => \&rw_git,
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
# anchors there but literal characters in aitools.
sub escapes_safe {
  my ($p) = @_;
  while ($p =~ /\\(.)/g) {
    return 0 unless $1 =~ /[bBdDsSwW]|[^A-Za-z0-9<>`']/;
  }
  return 1;
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
  if ($opt->{files}) {
    push @out, '--output', 'files', '--limit', '200';
  } elsif ($opt->{count}) {
    push @out, '--output', 'count', '--limit', '200';
  } else {
    push @out, '--context', $opt->{context} // 0;
    push @out, '--before', $opt->{before} if defined $opt->{before};
    push @out, '--after', $opt->{after} if defined $opt->{after};
    push @out, '--limit', '100';
  }
  push @out, '--glob', $opt->{glob} if defined $opt->{glob};
  push @out, '--no-ignore' if $opt->{no_ignore};
  return @out;
}

sub rw_grep {
  my %short = (
    n => '', H => '', s => '', I => '', r => 'recursive', R => 'recursive',
    i => 'icase', w => 'word', F => 'fixed', E => 'extended', l => 'files', c => 'count',
    e => pattern_setter(), A => context_setter('after'), B => context_setter('before'),
    C => context_setter('context'),
  );
  my %long = (
    'line-number' => '', recursive => 'recursive', 'ignore-case' => 'icase',
    'word-regexp' => 'word', 'fixed-strings' => 'fixed', 'extended-regexp' => 'extended',
    'files-with-matches' => 'files', count => 'count', regexp => pattern_setter(),
    include => glob_setter(),
  );
  my ($opt, $patterns, $paths) = search_args(\%short, \%long, @_) or return ();
  # Without a path grep reads stdin, which a hook cannot supply.
  return () if !@$paths && !$opt->{recursive};
  return () if $opt->{files} && $opt->{count};
  unless ($opt->{fixed}) {
    for my $p (@$patterns) {
      return () unless $opt->{extended} ? escapes_safe($p) : bre_safe($p);
    }
  }
  # grep reads every file it is pointed at; aitools, like rg, skips .gitignore'd ones by default.
  $opt->{no_ignore} = 1;
  return search_command($opt, $patterns, $paths);
}

sub rw_rg {
  my %short = (
    n => '', N => '', i => 'icase', w => 'word', F => 'fixed', l => 'files', c => 'count',
    e => pattern_setter(), A => context_setter('after'), B => context_setter('before'),
    C => context_setter('context'),
    g => glob_setter(),
  );
  my %long = (
    'line-number' => '', 'no-heading' => '', 'ignore-case' => 'icase', 'word-regexp' => 'word',
    'fixed-strings' => 'fixed', 'files-with-matches' => 'files', count => 'count',
    regexp => pattern_setter(), glob => $short{g},
  );
  my ($opt, $patterns, $paths) = search_args(\%short, \%long, @_) or return ();
  return () if $opt->{files} && $opt->{count};
  unless ($opt->{fixed}) {
    for my $p (@$patterns) { return () unless escapes_safe($p) }
  }
  return search_command($opt, $patterns, $paths);
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
  # ls hides dot entries unless -a or -A; aitools find lists them.
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
