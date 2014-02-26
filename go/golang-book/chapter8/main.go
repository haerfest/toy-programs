package main

import "fmt"

func swap(x *int, y *int) {
	z := *x
	*x = *y
	*y = z
}

func main() {
	x := 1
	y := 2

	swap(&x, &y)

	fmt.Println("x:", x, "y:", y)
}
