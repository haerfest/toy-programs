package main

import (
	"fmt"
	"math/big"
)

type fibGen func() *big.Int

func makeFibGen() fibGen {
	a := big.NewInt(0)
	b := big.NewInt(1)
	var c big.Int
	
	return func() *big.Int {
		c.Set(b)
		b.Add(a, b)
		return a.Set(&c)
	}
}

func fib1k() uint {
	var limit big.Int
	limit.Exp(big.NewInt(10), big.NewInt(999), nil)

	gen := makeFibGen()
	n := uint(1)
	for limit.Cmp(gen()) > 0 {
		n++
	}
	
	return n
}

func main() {
	fmt.Println(fib1k())  // => 4782
}











