#lang racket

; Pythagorean triplet: a^2 + b^2 = c^2
; There's one triplet a + b + c = 1000
; Find product abc

(define (brute-force)
  (for*/first ([a (in-range 1 1001)]
               [b (in-range a 1001)]
               [c (in-range (- 1000 a b) (- 1001 a b))]
               #:when (= (sqr c) (+ (sqr a) (sqr b))))
    (* a b c)))

; http://en.wikipedia.org/wiki/Pythagorean_triplet#Generating_a_triple
;
; m > n
;
; a = m^2 - n^2
; b = 2mn
; c = m^2 + n^2
;
; m^2 - n^2 + 2mn + m^2 + n^2 = 1000
; 2(m^2) + 2mn = 1000
; m^2 + mn = 500
; m(m + n) = 500
;
; m > n
; n < m
; n(n + n) < 500
; 2(n^2) < 500
; n^2 < 250
; n < 16

(define (quicker)
  (define (n-for m)
    (for/first ([n (in-range 1 (min m 16))]
                #:when (= 500 (* m (+ m n))))
      n))
  (define (try m)
    (let ([n (n-for m)])
      (if n
          (let ([a (- (sqr m) (sqr n))]
                [b (* 2 m n)]
                [c (+ (sqr m) (sqr n))])
            (* a b c))
          (try (add1 m)))))
  (try 2))
  

            
          
