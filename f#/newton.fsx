(* Computes the root of a function using the Newton-Raphson method *)

let newton f initialGuess =
    let dx = 0.001
    let f' x = f x / dx
    let eps = 0.001

    let rec check guess =
        let diff = abs (f guess)
        if diff < eps then
            guess
        else
            let improvedGuess = guess + (f guess) / (f' guess)
            check improvedGuess

    check initialGuess

let newtonSqrt n =
    newton (fun (x) -> x * x - n) 1.0

// Example session:
// > newtonSqrt 9.0 ;;
// val it : float = 3.0
