#lang racket

; the Fibonacci sequence:
;
; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;
; recursive definition:
;
; fib(1) = 1
; fib(2) = 2
; fib(n) = fib(n - 1) + fib(n - 2), n > 2
;
; find the sum of the even-valued fib. numbers <= 4,000,000
;
; at all times we only need to two most recent Fibonacci numbers,
; so no need to build the entire list first, then filter, then sum
; an iterative approach is much more efficient

(define (sum-of-even-fibs upper-limit)
  (define (iterate prev-fib curr-fib sum-even-fibs)
    (let ([next-fib (+ prev-fib curr-fib)])
      (if (> next-fib upper-limit)
          sum-even-fibs
          (iterate curr-fib next-fib (if (even? next-fib)
                                         (+ sum-even-fibs next-fib)
                                         sum-even-fibs)))))
  (iterate 1 2 2))

(sum-of-even-fibs 4000000)  ; => 4613732

; interesting observations on the thread:
;
; 1. the ratio between two consequtive numbers approaches the golden ratio
; 2. each third number is even
;
; if we go with #2, then if we start the sequence with a and b:
;
; a
; b
; b + a
; b + a + b = 2b + a
; 2b + a + b + a = 3b + 2a
;
; so if we have an a and b, we can get to the next even number via
; 2a + 3b, and the odd number right before it via a + 2b.

(define (sum-of-even-fibs2 upper-limit)
  (define (iterate prev-fib curr-fib sum-even-fibs)
    (let ([next-even-fib (+ (* 3 curr-fib) (* 2 prev-fib))]
          [odd-fib-before-it (+ (* 2 curr-fib) prev-fib)])
      (if (> next-even-fib upper-limit)
          sum-even-fibs
          (iterate odd-fib-before-it
                   next-even-fib
                   (+ sum-even-fibs next-even-fib)))))
  (iterate 1 2 2))

(sum-of-even-fibs2 4000000)  ; => 4613732
