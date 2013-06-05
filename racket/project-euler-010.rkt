#lang racket

(define (sieve upper-limit)
  (let ([numbers (make-vector upper-limit 'unmarked)])
    
    (define (mark-multiples-of n)
      (let loop ([number (sqr n)])
        (when (< number (vector-length numbers))
          (vector-set! numbers number 'marked)  ; number is not prime
            (loop (+ number n)))))
    
    (define (find-next-number-to-mark number)
      (let loop ([candidate number])
        (cond [(>= (sqr candidate) (vector-length numbers)) 0]  ; done with numbers
              [(eq? (vector-ref numbers candidate) 'unmarked) candidate]
              [else (loop (add1 candidate))])))
    
    (define (sum-primes)
      (let loop ([number 2]
                 [sum 0])
        (if (>= number (vector-length numbers))
            sum
            (loop (add1 number)
                  (if (eq? (vector-ref numbers number) 'unmarked)
                      (+ sum number)
                      sum)))))
    
    (let loop ([number 2])
      (if (zero? number)
          (sum-primes)
          (begin
            (mark-multiples-of number)
            (loop (find-next-number-to-mark (add1 number))))))))

(sieve 2000000)  ; => 142913828922