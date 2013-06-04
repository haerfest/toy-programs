#lang racket

; What is the 10,001st prime number?

(define (trial-division nth)
  (define (prime? n)
    (let ([upper-bound (floor (sqrt n))])
      (let loop ([divisor 2])
        (cond [(> divisor upper-bound) #t]
              [(zero? (remainder n divisor)) #f]
              [else (loop (add1 divisor))]))))
  (if (= nth 1)
      2
      (let loop ([candidate 3]
                 [remaining (sub1 nth)])
        (if (prime? candidate)
            (if (= 1 remaining)
                candidate
                (loop (+ candidate 2) (sub1 remaining)))
            (loop (+ candidate 2) remaining)))))

(trial-division 10001)  ; => 104743


(define (sieve nth upper-limit)
  (let ([numbers (make-vector upper-limit 'unmarked)])
    
    (define (mark-multiples-of n)
      (let loop ([multiplier 2])
        (let ([number (* multiplier n)])
          (when (< number (vector-length numbers))
            (vector-set! numbers number 'marked)  ; number is not prime
            (loop (add1 multiplier))))))
    
    (define (find-next-number-to-mark number)
      (let loop ([candidate number])
        (cond [(>= candidate (vector-length numbers)) 0]  ; done with numbers
              [(eq? (vector-ref numbers candidate) 'unmarked) candidate]
              [else (loop (add1 candidate))])))
    
    (define (nth-prime)
      (let loop ([remaining (sub1 nth)]
                 [number    2])
        (cond [(= number (vector-length numbers)) #f]  ; desired prime not found
              [(eq? (vector-ref numbers number) 'unmarked)
               (if (zero? remaining)
                   number
                   (loop (sub1 remaining) (add1 number)))]
              [else (loop remaining (add1 number))])))
    
    (let loop ([number 2])
      (if (zero? number)
          (nth-prime)
          (begin
            (mark-multiples-of number)
            (loop (find-next-number-to-mark (add1 number))))))))

(sieve 10001 105000)  ; => 104743
  