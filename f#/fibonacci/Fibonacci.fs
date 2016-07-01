module Fibonacci

open System
open System.Diagnostics

let rec fib n =
    if n < 2 then
        1
    else
        fib (n - 2) + fib (n - 1)

let fibImperative n =
    let mutable a = 1
    let mutable b = 1
    for _ in 1..n do
        let c = a + b
        a <- b
        b <- c
    a

let timeIt f =
    let stopwatch = Stopwatch.StartNew()
    let result = f()
    stopwatch.Stop()
    stopwatch.Elapsed, result

[<EntryPoint>]
let main args =
    for arg in args do
        let ok, n = Int32.TryParse arg
        if ok then
            let elapsed, result = timeIt (fun () -> fib n)
            printfn "recursive  fib(%d) = %d [%3s]" n result (elapsed.ToString())
            let elapsed, result = timeIt (fun () -> fibImperative n)
            printfn "imperative fib(%d) = %d [%3s]" n result (elapsed.ToString())
    0

// Example output:
//
// $ mono bin/Release/Fibonacci.exe 39 40 41
// recursive  fib(39) = 102334155 [00:00:00.5300601]
// imperative fib(39) = 102334155 [00:00:00.0000702]
// recursive  fib(40) = 165580141 [00:00:00.8508038]
// imperative fib(40) = 165580141 [00:00:00.0000004]
// recursive  fib(41) = 267914296 [00:00:01.3792586]
// imperative fib(41) = 267914296 [00:00:00.0000003]
