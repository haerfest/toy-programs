// Solution to Project Euler 26 in ugly Go... :(
//
// Explanation of the math behind it found at [1].  Styled after another
// user's very elegant Haskell solution which follows the same logic.
//
// [1] http://answers.yahoo.com/question/index?qid=20060812092021AAetJwP

package main

import (
	"fmt"
	"math/big"
)

func main() {
	one  := big.NewInt(1)
	zero := big.NewInt(0)
	ten  := big.NewInt(10)

	var a, pow, bigP big.Int
	var n int
	
	maxRecurringCycleLength := func (p int) int {
		bigP.SetInt64(int64(p))

		n = 1
		pow.SetInt64(int64(1))
		
		for a.Mod(a.Sub(&pow, one), &bigP).Cmp(zero) > 0 {
			n++
			pow.Mul(&pow, ten)
		}

		return n
	}

	max, dmax := 0, 0

	for d := 3; d < 1000; d += 2 {
		if d % 5 == 0 {
			continue
		}

		length := maxRecurringCycleLength(d)
		if length > max {
			max, dmax = length, d
		}
	}

	fmt.Println(dmax)  // => 983
}
