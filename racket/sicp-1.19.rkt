#lang racket

; SICP exercise 1.19:
;
; Tpq(a,b) : a <- bq + aq + ap
;            b <- bp + aq
; 
; Tpq(Tpq(a,b)) : a  <- b'q + a'q + a'p
;                 b  <- b'p + a'q
;                 a' <- bq + aq + ap
;                 b' <- bp + aq
; 
;                 a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;                 b <- (bp + aq)p + (bq + aq + ap)q
; 
;                 a <- bpq + aqq + bqq + aqq + apq + bqp + aqp + app
;                 b <- bpp + aqp + bqq + aqq + apq
; 
;                 a <- app + apq + apq + aqq + aqq +       bpq + bpq + bqq
;                 b <-       apq + apq + aqq +       bpp +             bqq
; 
;                 a <- b(     pq + pq + qq) + a(pp + pq + pq + qq + qq)
;                 b <- b(pp +           qq) + a(     pq + pq +      qq)
; 
;                 a <- b(     pq + pq + qq) + a(pq + pq + qq) + a(pp + qq)
;                 b <- b(pp +           qq) + a(     pq + pq +      qq)
; 
;                 a  <- bq' + aq' + ap'
;                 b  <- bp' + aq'
;                 p' <- pp + qq
;                 q' <- pq + pq + qq

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ([= count 0] b)
        ([even? count] (fib-iter a
                                 b
                                 (+ (* p p) (* q q))    ; = p'
                                 (+ (* 2 p q) (* q q))  ; = q'
                                 (/ count 2)))
        (else (fib-iter (+ (* b q)
                           (* a q)
                           (* a p))
                        (+ (* b p)
                           (* a q))
                        p
                        q
                        (- count 1)))))
                                 