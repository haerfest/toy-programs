let rec fib n =
    if n < 2 then 1 else fib (n - 2) + fib (n - 1)

let timeIt f =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let result = f()
    stopwatch.Stop()
    printfn "elapsed: %d msec" stopwatch.Elapsed.Milliseconds

let main () =
    timeIt (fun () -> fib 40)

