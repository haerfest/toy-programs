package main

import (
	"fmt"
	"math/big"
	"strings"
)

const precision = 2000

func maxRecurringCycleLength(x *big.Rat) int {
	fraction := x.FloatString(precision)[2:]
	
	for i := 0; i < len(fraction); i++ {
		for j := i + 1; j < len(fraction); j++ {
			cycle := fraction[i:j]
			n := len(fraction[j:]) / len(cycle)			
			if n > 0 {
				repeated := strings.Repeat(cycle, n)
				if fraction[j:j+n*(j-i)] == repeated {
					return j - i
				}
			}
		}
	}
	
	return 0
}

func main() {
	max, d := 0, 0

	for i := 2; i < 1000; i++ {
		x := big.NewRat(1, int64(i))
		length := maxRecurringCycleLength(x)
		if length > max {
			max, d = length, i
		}
	}

	fmt.Println(d)  // => 983
}
