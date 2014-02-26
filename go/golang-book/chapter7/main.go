package main

import "fmt"

func fac(n uint64) uint64 {
	if n == 0 {
		return 1
	}

	return n * fac(n - 1)
}

func main() {
	fmt.Print("Enter number: ")

	var n uint64
	fmt.Scanf("%d", &n)

	fmt.Printf("%d! = %d\n", n, fac(n))
}
	
