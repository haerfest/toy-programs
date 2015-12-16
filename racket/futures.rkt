#lang racket

;; -----------------------------------------------------------------------------
;;  Futures example from:
;;  http://docs.racket-lang.org/guide/parallelism.html
;; -----------------------------------------------------------------------------

(provide main)

(define (main . args)
  (for ((arg args))
    (case arg
      (("mandelbrot") (test mandelbrot))
      (("mandelbrot1") (test mandelbrot1))
      (("mandelbrot2") (test mandelbrot2)))))

(define (test m)
  (display "Sequential #1 ..... ")
  (time (m 10000000 62 500 1000))
  (display "Sequential #2 ..... ")
  (time (m 10000000 62 501 1000))
  (display "Sequential list ... ")
  (time (list (m 10000000 62 500 1000)
              (m 10000000 62 501 1000)))
  (display "Parallel .......... ")
  (time (let ([f (future (lambda () (m 10000000 62 501 1000)))])
          (list (m 10000000 62 500 1000)
                (touch f)))))

;; -----------------------------------------------------------------------------
;;  Initial version.
;; -----------------------------------------------------------------------------

; $ racket -tm futures.rkt mandelbrot
;
; Sequential #1 ..... cpu time: 915 real time: 916 gc time: 174
; Sequential #2 ..... cpu time: 766 real time: 766 gc time: 15
; Sequential list ... cpu time: 1507 real time: 1506 gc time: 41
; Parallel .......... cpu time: 1501 real time: 1505 gc time: 31;
;
; Mixing floating-point and integer operations blocks the future and makes
; Racket run them in sequence instead of parallel.

(define (mandelbrot iterations x y n)
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]
        [cr (- (/ (* 2.0 x) n) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (* zr zr)]
                [ziq (* zi zi)])
            (cond
              [(> (+ zrq ziq) 4) i]
              [else (loop (add1 i)
                          (+ (- zrq ziq) cr)
                          (+ (* 2 zr zi) ci))]))))))

;; -----------------------------------------------------------------------------
;;  Don't mix integer and floating-point calculations, it's slow.
;; -----------------------------------------------------------------------------

; $ racket -tm futures.rkt mandelbrot1
;
; Sequential #1 ..... cpu time: 445 real time: 446 gc time: 167
; Sequential #2 ..... cpu time: 278 real time: 278 gc time: 14
; Sequential list ... cpu time: 572 real time: 572 gc time: 29
; Parallel .......... cpu time: 1731 real time: 1342 gc time: 49
;
; The many floating-point values that are created are being boxed all the time,
; causing many memory allocations which force synchronization events.

(define (mandelbrot1 iterations x y n)
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]
        [cr (- (/ (* 2.0 x) n) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (* zr zr)]
                [ziq (* zi zi)])
            (cond
              [(> (+ zrq ziq) 4.0) i]
              [else (loop (add1 i)
                          (+ (- zrq ziq) cr)
                          (+ (* 2.0 zr zi) ci))]))))))

;; -----------------------------------------------------------------------------
;;  Avoid boxing floating-point numbers by using flonum-specific operations.
;; -----------------------------------------------------------------------------

; $ racket -tm futures.rkt mandelbrot2
;
; Sequential #1 ..... cpu time: 92 real time: 93 gc time: 0
; Sequential #2 ..... cpu time: 91 real time: 91 gc time: 0
; Sequential list ... cpu time: 182 real time: 183 gc time: 0
; Parallel .......... cpu time: 173 real time: 92 gc time: 0

(require racket/flonum)

(define (mandelbrot2 iterations x y n)
  (let ([ci (fl- (fl/ (* 2.0 (->fl y)) (->fl n)) 1.0)]
        [cr (fl- (fl/ (* 2.0 (->fl x)) (->fl n)) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (fl* zr zr)]
                [ziq (fl* zi zi)])
            (cond
              [(fl> (fl+ zrq ziq) 4.0) i]
              [else (loop (add1 i)
                          (fl+ (fl- zrq ziq) cr)
                          (fl+ (fl* 2.0 (fl* zr zi)) ci))]))))))
