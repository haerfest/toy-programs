(use http-client uri-common)

(define (random-reddit n)
  (define (iter n acc)
    (if (zero? n)
        acc
        (let-values (((html uri response)
                      (with-input-from-request "http://www.reddit.com/r/random/" #f read-string)))
          (iter (- n 1)
                (cons (uri->string uri) acc)))))
  (iter n '()))

(define (main args)
  (let ((n (if (null? args)
               1
               (string->number (car args)))))
    (map (lambda (s)
           (display s)
           (newline))
         (random-reddit n))))

(main (command-line-arguments))
