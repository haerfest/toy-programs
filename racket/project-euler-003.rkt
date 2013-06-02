#lang racket

; how to efficiently calculate the largest prime factor of a number...
;
;   9 = 3 * 3 * 1, so 3
;  10 = 5 * 2 * 1, so 5
;  11 = 11 * 1, so 11
; 100 = 5 * 5 * 2 * 2 * 1, so 5
;
; logic: 100 = 100 * 1
;            = (50 * 2) * 1
;            = (25 * 2) * 2 * 1
;            = (5 * 5) * 2 * 2 * 1
;
; prime means its only divisible by 1 and itself, no other factors


(define (largest-prime-factor n)
  (define (smallest-divisor-not-one)
    (let loop ([divisor 2])
      (if (zero? (remainder n divisor))
          divisor
          (loop (add1 divisor)))))
  (let ([divisor (smallest-divisor-not-one)])
    (if (= divisor n)
        n
        (largest-prime-factor (/ n divisor)))))

(largest-prime-factor 600851475143)  ; => 6857