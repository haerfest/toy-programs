(define continuation '())

(define (foo n)
  (* (call-with-current-continuation
      (lambda (c)
        (set! continuation c)
        (+ n 1)))
     2))

