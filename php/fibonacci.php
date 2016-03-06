<?php

function fib($n) {
    if ($n < 2) {
        return 1;
    }
    return fib($n - 2) + fib($n - 1);
}

$start = microtime(true);
$answer = fib(40);
$elapsed = microtime(true) - $start;

print "$answer, elapsed: $elapsed\n";
?>
