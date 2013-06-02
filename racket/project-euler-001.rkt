#lang racket

; multiples of 3: 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, ...
; multiples of 5: 5, 10, 15, 20, 25, 30, ...
;
; 15 and 30 occur in both sets, so multiples of 3 * 5 should
; not be counted
;
; sum of multiples of 3 below 10:
;
; 1 * 3 = 3
; 2 * 3 = 6
; 3 * 3 = 9
;        -- +
;        18
;
; largest factor to multiply 3 with is 3, which is (sub1 (ceiling (/ 10 3)))
;
; so can we sum 1 * 3 + 2 * 3 + 3 * 3 in one step?
; is it not (1 + 2 + 3) * 3?
; can we sum 1 + 2 + 3 in one step, knowing 3 as our largest factor?
;
; Googling for "1 + 2 + 3" leads us to:
;
; http://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_⋯
;
; Which tells us:
;                       n (n + 1)
; 1 + 2 + 3 + ... + n = ---------
;                          2

(define (sum-of-multiples upper-limit)  
  (define (sum-of-multiples-of n)
    (let* ([max-factor     (sub1 (ceiling (/ upper-limit n)))]
           [factors-summed (/ (* max-factor (add1 max-factor)) 2)])
      (* factors-summed n)))
  (- (+ (sum-of-multiples-of 3) (sum-of-multiples-of 5))
     (sum-of-multiples-of 15)))

(sum-of-multiples 1000)  ; => 233168