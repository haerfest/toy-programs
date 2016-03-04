package main

import (
    "fmt"
    "time"
)

func timeIt(f func()) {
    start := time.Now()
    f()
    elapsed := time.Since(start)
    fmt.Println("Elapsed:", elapsed)
}

func fib(n int) int {
    if (n < 2) {
        return 1
    }
    return fib(n - 1) + fib(n - 2)
}

func main() {
    timeIt(func() { fib(40) })
}
