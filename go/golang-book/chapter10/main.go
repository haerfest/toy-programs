package main

import (
	"fmt"
	"time"
	"math/rand"
)

const goroutines = 10

func f(n int, done chan bool) {
	for i := 0; i < 10; i++ {
		fmt.Println(n, ":", i)
		amt := time.Duration(rand.Intn(250))
		time.Sleep(time.Millisecond * amt)
	}

	done <- true
}

func main() {
	done := make(chan bool)

	for i := 0; i < goroutines; i++ {
		go f(i, done)
	}

	for i := 0; i < goroutines; i++ {
		<- done
	}
}
