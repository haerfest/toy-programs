(* To compile: ocamlbuild -lib Unix fibonacci.native *)

let rec fib n =
    if n < 2 then 1
    else fib (n - 2) * fib (n - 1)

let time f x =
    let t = Unix.gettimeofday() in
    let fx = f x in
    Printf.printf "elapsed: %f sec\n" (Unix.gettimeofday() -. t);
    fx

let _ = time fib 40
