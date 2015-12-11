#lang racket

; > (time (fib 40))
; cpu time: 10407 real time: 10405 gc time: 24
; 165580141
; > (time (memoized-fib 40))
; cpu time: 1 real time: 0 gc time: 0
; 165580141

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (memoized-fib n)
  (if (< n 2)
      1
      (let ((memory (make-hash '((0 . 1) (1 . 1)))))
        (define (fib n)
          (hash-ref! memory n
                     (λ () (+ (fib (- n 1))
                              (fib (- n 2))))))
        (fib n))))
