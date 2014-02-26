package main

import "fmt"

func Divide(numerator int, divisor int) float64 {
	if divisor == 0 {
		panic("division by zero")
	}

	return float64(numerator) / float64(divisor)
}

func main() {
	defer func() {
		s := recover()
		fmt.Println("Panic:", s)
	}()

	fmt.Println("10 / 3 =", Divide(10, 3))
	fmt.Println("10 / 0 =", Divide(10, 0))
}
	
