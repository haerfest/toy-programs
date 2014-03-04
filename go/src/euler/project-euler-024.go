package main

import (
	"fmt"
	"sort"
)

type Perm struct {
	perm []int
}

type Perms []Perm

var digits = []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
var perms = make(Perms, 0, 3628800)

func (this Perm) value() int {
	v := 0
	for _, digit := range this.perm {
		v = v * 10 + digit
	}
	return v
}

func (this Perms) Len() int {
	return len(this)
}

func (this Perms) Less(i, j int) bool {
	return this[i].value() < this[j].value()
}

func (this Perms) Swap(i, j int) {
	this[i], this[j] = this[j], this[i]
}

func newPerm(xs []int) Perm {
	p := Perm{perm: make([]int, len(xs))}
	copy(p.perm, xs)
	return p
}

func add(xs []int) {
	i := len(perms)
	perms = perms[:i+1]
	perms[i] = newPerm(xs)
}

func permute(xs []int, start int) {
	if start == len(xs) - 1 {
		add(xs)
	}

	for i := start; i < len(xs); i++ {
		if i > start {
			xs[start], xs[i] = xs[i], xs[start]
		}

		permute(xs, start + 1)
		
		if i > start {
			xs[start], xs[i] = xs[i], xs[start]
		}
	}
}


func main() {
	permute(digits, 0)
	sort.Sort(perms)
	fmt.Println(perms[999999])  // => {[2 7 8 3 9 1 5 4 6 0]}
}
