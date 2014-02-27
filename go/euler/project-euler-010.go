package main

import "fmt"

func Sieve(upperLimit uint) uint {
	marked := make([]bool, upperLimit)

	markMultiplesOf := func(n uint) {
		for number := n * n; number < upperLimit; number += n {
			marked[number] = true
		}
	}

	findNextNumberToMark := func(candidate uint) uint {
		for candidate * candidate < upperLimit && marked[candidate] {
			candidate++
		}

		if !marked[candidate] {
			return candidate
		}

		return 0
	}

	sumPrimes := func() uint {
		sum := uint(0)
		for number := uint(2); number < upperLimit; number++ {
			if !marked[number] {
				sum += number
			}
		}

		return sum
	}

	for number := uint(2); number != 0; number = findNextNumberToMark(number + 1) {
		markMultiplesOf(number)
	}

	return sumPrimes()
}

func main() {
	fmt.Println(Sieve(2000000))  // => 142913828922
}
