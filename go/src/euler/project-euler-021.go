/* Let d(n) be defined as the sum of proper divisors of n (numbers
 * less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable
 * pair and each of a and b are called amicable numbers.
 *
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
 * 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of
 * 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 */

package main

import (
	"fmt"
	"math"
)

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

func sumOfAmicablePairs(limit int) int {
	sum := 0
	for n := 2; n < limit; n++ {
		m := sumOfProperDivisors(n)
		if m != n && n == sumOfProperDivisors(m) {
			sum += n
		}
	}
	return sum
}

func main() {
	fmt.Println(sumOfAmicablePairs(10000))  // => 31626
}
