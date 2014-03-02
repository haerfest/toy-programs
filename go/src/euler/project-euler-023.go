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

package main

import (
	"fmt"
	"math"
	"sort"
)

const limit = 28124 
var isAbundant = make(map[int]bool)
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
		if isAbundant[n] {
			// we've already found this one and its multiples
			continue
		}
		sum := sumOfProperDivisors(n)
		if sum == n {
			// a perfect number: its multiples are abundants
			markMultiplesOf(n)
		} else if sum > n {
			// an abundant number: its multiples are abundants too
			isAbundant[n] = true
			markMultiplesOf(n)
		}
	}
}

func extractAbundantNumbersSorted() []int {
	abundants := make([]int, len(isAbundant))
	i := 0
	for a := range isAbundant {
		abundants[i] = a
		i++
	}
	sort.Ints(abundants)
	return abundants
}

func markSumsOfTwoAbundantNumbers() {
	abundants := extractAbundantNumbersSorted()
	for i, a := range abundants {
		for j := i; j < len(abundants); j++ {
			b := abundants[j]
			if a + b >= limit {
				break
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
	fmt.Println(sumAllNonSumsOfTwoAbundantNumbers())  // => 4179871
}
