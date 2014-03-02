/* A perfect number is a number for which the sum of its proper divisors is
 * exactly equal to the number. For example, the sum of the proper divisors
 * of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
 * number.
 *
 * A number n is called deficient if the sum of its proper divisors is less
 * than n and it is called abundant if this sum exceeds n.
 *
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
 * number that can be written as the sum of two abundant numbers is 24. By
 * mathematical analysis, it can be shown that all integers greater than 28123
 * can be written as the sum of two abundant numbers. However, this upper limit
 * cannot be reduced any further by analysis even though it is known that the
 * greatest number that cannot be expressed as the sum of two abundant numbers
 * is less than this limit.
 *
 * Find the sum of all the positive integers which cannot be written as the sum
 * of two abundant numbers.
 */

/* http://primes.utm.edu/glossary/xpage/AbundantNumber.html
 *
 * Every proper multiple of a perfect number, and every multiple of an abundant
 * number, is abundant.
 */

package main

import (
	"fmt"
	"math"
)

const limit = 28124 
var isAbundant = make([]bool, limit)
var isSumOfTwo = make([]bool, limit)

func sumOfProperDivisors(n int) int {
	limit := int(math.Ceil(math.Sqrt(float64(n))))
	sum := 1
	for divisor := 2; divisor < limit; divisor++ {
		if n % divisor == 0 {
			sum += divisor + n / divisor
		}
	}
	return sum
}

func markMultiplesOf(n int) {
	for factor := 2; n * factor < limit; factor++ {
		isAbundant[n * factor] = true
	}
}

func markAbundantNumbers() {
	for n := 2; n < limit; n++ {
		sum := sumOfProperDivisors(n)
		if sum == n {
			markMultiplesOf(n)
		} else if sum > n {
			isAbundant[n] = true
			markMultiplesOf(n)
		}
	}
}

func markSumsOfTwoAbundantNumbers() {
	for a := 0; a < limit; a++ {
		if !isAbundant[a] {
			continue
		}
		for b := a; a + b < limit; b++ {
			if !isAbundant[b] {
				continue
			}
			isSumOfTwo[a + b] = true
		}
	}
}

func sumAllNonSumsOfTwoAbundantNumbers() int {
	sum := 0
	for n := 1; n < limit; n++ {
		if !isSumOfTwo[n] {
			sum += n
		}
	}
	return sum
}

func main() {
	markAbundantNumbers()
	markSumsOfTwoAbundantNumbers()
	fmt.Println(sumAllNonSumsOfTwoAbundantNumbers())
}
