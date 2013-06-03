#lang racket

; smallest pos. number evenly divisible by 1..20
;
; 2520 is that for 1..10
;
; (map (lambda (x) (/ 2520 x)) '(1 2 3 4 5 6 7 8 9 10))
; =>
; '(2520 1260 840 630 504 420 360 315 280 252)
;
; we're looking for x such that:
;
; x = a * 20
;   = b * 19
;   = c * 18
;   ...
;   = t * 1
;

(define (answer max-factor)
  (let ([all-factors (sequence->list (in-range 2 (add1 max-factor)))])
    
    (define (smallest-factor-of n)
      (let loop ([factors all-factors])
        (if (empty? factors)
            n
            (let ([factor (first factors)])
              (if (zero? (remainder n factor))
                  factor
                  (loop (rest factors)))))))
    
    (let loop ([number (first all-factors)]
               [factors (rest all-factors)])
      (if (empty? factors)
          number
          (let ([factor (first factors)])
            (loop (if (zero? (remainder number factor))
                      number
                      (* number (smallest-factor-of factor)))
                  (rest factors)))))))

(answer 20)  ; => 232792560