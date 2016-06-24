(* Computes the square root of a number using the Newton-Raphson method *)

let newtonSqrt n =
    let initialGuess = 1.0
    let f x = x * x - n    // the fn to find the root of
    let f' x = 2.0 * x     // its derivative
    let eps = 0.001        // the desired accuracy

    let rec inner guess =
        let diff = abs (f guess)
        if diff < eps then
            guess
        else
            let improvedGuess = guess - (f guess) / (f' guess)
            inner improvedGuess

    inner initialGuess

(* Example session:
 *
 * > newtonSqrt 2.0;;
 * val it : float = 1.414215686
 *)
