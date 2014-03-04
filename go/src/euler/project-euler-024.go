package main

import (
	"fmt"
	"sort"
)

type permutations [][]int

var digits = []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
var perms = make(permutations, 0, 3628800)

func (this permutations) Len() int {
	return len(this)
}

func (this permutations) Less(i, j int) bool {
	return value(this[i]) < value(this[j])
}

func (this permutations) Swap(i, j int) {
	this[i], this[j] = this[j], this[i]
}

func value(xs []int) int {
	v := 0
	for _, digit := range xs {
		v = v * 10 + digit
	}
	return v
}

func copyXs(xs []int) []int {
	ys := make([]int, len(xs))
	copy(ys, xs)
	return ys
}

func add(xs []int) {
	i := len(perms)
	perms = perms[:i+1]
	perms[i] = copyXs(xs)
}

func permute(xs []int, start int) {
	if start == len(xs) - 1 {
		add(xs)
	}

	for i := start; i < len(xs); i++ {
		xs[start], xs[i] = xs[i], xs[start]
		permute(xs, start + 1)
		xs[start], xs[i] = xs[i], xs[start]
	}
}


func main() {
	permute(digits, 0)
	sort.Sort(perms)
	fmt.Println(perms[999999])  // => {[2 7 8 3 9 1 5 4 6 0]}
}
