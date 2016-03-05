#!/usr/bin/env perl
use strict;
use warnings;

use Time::HiRes qw( time );

sub fib {
    my $n = $_[0];
    return 1 if $n < 2;
    fib($n - 2) + fib($n - 1);
}

my $start = time();
my $answer = fib(40);
my $elapsed = time() - $start;

printf "%d, elapsed: %.3f secs\n", $answer, $elapsed;
