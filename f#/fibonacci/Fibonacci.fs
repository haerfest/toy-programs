module Fibonacci

open System
open System.Diagnostics

let rec fib n =
    if n < 2 then
        1
    else
        fib (n - 2) + fib (n - 1)

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
            printfn "fib(%d) = %d [%3s]" n result (elapsed.ToString())
    0

(* Example output:
 *
 * $ mono bin/Release/Fibonacci.exe 39 40 41
 * fib(39) = 102334155 [00:00:00.5880708]
 * fib(40) = 165580141 [00:00:00.9590467]
 * fib(41) = 267914296 [00:00:01.5658315]
 *)
