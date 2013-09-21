#lang racket

; find the largest numerical palindrome (e.g. 9009) from the
; product of two 3-digit numbers

(define (digits n)
  (let loop ([m n]
             [digit-list '()])
    (let* ([last-digit       (remainder m 10)]
           [remaining-digits (quotient m 10)]
           [new-list         (append digit-list (list last-digit))])
      (if (zero? remaining-digits)
          new-list  ; could reverse here, but not necessary for correctness
          (loop remaining-digits new-list)))))

(define (palindrome? n)
  (equal? (digits n) (reverse (digits n))))

(define (largest-product-palindrome)
  (apply max (for*/list ([a (in-range 999 99 -1)]
                         [b (in-range a 99 -1)]
                         #:when (palindrome? (* a b)))
               (* a b))))

(largest-product-palindrome)  ; => 906609