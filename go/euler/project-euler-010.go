package main

import "fmt"

func Sieve(upperLimit uint64) uint64 {
	marked := make([]bool, upperLimit)

	markMultiplesOf := func(n uint64) {
		for number := n * n; number < upperLimit; number += n {
			marked[number] = true
		}
	}

	findNextNumberToMark := func(candidate uint64) uint64 {
		for candidate * candidate < upperLimit && marked[candidate] {
			candidate++
		}

		if !marked[candidate] {
			return candidate
		}

		return 0
	}

	sumPrimes := func() uint64 {
		var sum uint64 = 0
		var number uint64

		for number = 2; number < upperLimit; number++ {
			if !marked[number] {
				sum += number
			}
		}

		return sum
	}

	var number uint64
	for number = 2; number != 0; number = findNextNumberToMark(number + 1) {
		markMultiplesOf(number)
	}

	return sumPrimes()
}

func main() {
	fmt.Println(Sieve(2000000))  // => 142913828922
}
