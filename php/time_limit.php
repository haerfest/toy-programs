<?php

//
// Ugly hack to keep track of jobs to finish nicely when we are
// being terminated because we have run for too long.
//

$jobs = [];

//
// When we're being terminated, grab the chance to do something.
//

register_shutdown_function('Shutdown');

function Shutdown() {
    global $jobs;
    print("Shutting down...\n");
    foreach ($jobs as $job) {
        print("Closed job $job\n");
    }
}

//
// Simulate a busy-wait, since any time spent in sleep() is not
// counted towards real processor time used.
//

function BusyWait($seconds) {
    $start = time();
    while (time() - $start < $seconds);
}

function NewJob() {
    $job = rand();
    BusyWait(1.0);
    return $job;
}

function LongComputation() {
    global $jobs;
    for ($i = 1; $i <= 60; $i++) {
        $job = NewJob();
        $jobs[] = $job;
        print("Created new job $job...\n");
    }
}

//
// Don't allow the program to run for more than three seconds.
//

set_time_limit(3);
LongComputation();

//
// Example run:
//
//   $ php time_limit.php 
//   Created new job 1014829188...
//   Created new job 1566941334...
//   Created new job 1223992867...
//
//   Fatal error: Maximum execution time of 3 seconds exceeded in time_limit.php on line 31
//   Shutting down...
//   Closed job 1014829188
//   Closed job 1566941334
//   Closed job 1223992867
//