package main

import "fmt"

// http://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
func permute(a []int, remaining int) []int {
	for remaining > 0 {

		// 1. find the largest index k such that a[k] < a[k + 1]
		k := len(a) - 2
		for k >= 0 && a[k] >= a[k + 1] {
			k--
		}

		// if no such index exists, the permutation is the last permutation
		if k < 0 {
			return a
		}

		// 2. find the largest index l such that a[k] < a[l]
		l := len(a) - 1
		for l >= 0 && a[k] >= a[l] {
			l--
		}

		// 3. swap the value of a[k] with that of a[l]
		a[k], a[l] = a[l], a[k]

		// 4. reverse the sequence from a[k + 1] up to and including the final element a[n]
		i := k + 1
		j := len(a) - 1
		for j > i {
			a[i], a[j] = a[j], a[i]
			i++
			j--
		}

		remaining--
	}

	return a
}


func main() {
	fmt.Println(permute([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, 999999))  // => [2 7 8 3 9 1 5 4 6 0]
}
