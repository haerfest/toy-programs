(use http-client uri-common)

(define (random-reddit)
  (let-values (((html uri response) (with-input-from-request "http://www.reddit.com/r/random/" #f read-string)))
    (display (uri->string uri))
    (newline)))

(define (random-reddits n)
  (when (> n 0)
        (random-reddit)
        (random-reddits (- n 1))))

(define (main args)
  (unless (null? args)
          (random-reddits (string->number (car args)))))

(main (command-line-arguments))
