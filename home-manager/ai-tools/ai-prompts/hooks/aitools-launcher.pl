package AitoolsLauncher;
use strict;
use warnings;
our %GLOBAL_VALUE = map { $_ => 1 } qw(--root --lock-timeout);
our %GLOBAL_FLAG = map { $_ => 1 } qw(--trace);
our %VALUE = map { $_ => 1 } qw(
    --stdin-data --stdin-file --cwd --env --timeout --head --tail --grep --grep-limit --stdout-to
    --name --root --lock-timeout
);
our %FLAG = map { $_ => 1 } qw(--stdin --no-strip-ansi --trace);
sub global_values { return %GLOBAL_VALUE; }
sub global_flags { return %GLOBAL_FLAG; }

# Option values consume a word even when that word is "--".
sub program_index {
    my @w = @_;
    my $j = 0;
    while ($j <= $#w) {
        return $j + 1 if $w[$j] eq '--';
        if ($VALUE{$w[$j]}) { $j += 2; next; }
        if ($FLAG{$w[$j]} or ($w[$j] =~ /^(--[a-z-]+)=/ and $VALUE{$1})) {
            $j++;
            next;
        }
        return $w[$j] =~ /^-/ ? undef : $j;
    }
    return undef;
}

sub launcher {
    my @w = @_;
    my $i = 0;
    while ($i <= $#w) {
        if ($GLOBAL_VALUE{$w[$i]}) { $i += 2; next; }
        if ($GLOBAL_FLAG{$w[$i]} or ($w[$i] =~ /^(--[a-z-]+)=/ and $GLOBAL_VALUE{$1})) {
            $i++;
            next;
        }
        last;
    }
    return { kind => 'batch' } if ($w[$i] // '') eq 'batch';
    my $start = ($w[$i] // '') eq 'run' ? $i + 1
        : ($w[$i] // '') eq 'bg' && ($w[$i + 1] // '') eq 'start' ? $i + 2 : undef;
    return {} unless defined $start;
    my $program = program_index(@w[$start .. $#w]);
    return { kind => 'run', index => defined $program ? $start + $program : undef };
}

unless (caller) {
    require JSON::PP;
    print JSON::PP::encode_json(launcher(@ARGV));
}
1;
