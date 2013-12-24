(require-extension zmq)
(require-extension posix)

(define (subscribe endpoint fn)

  (define (receive-loop s)
    (let* ([t (receive-message* s as: 'string)]
           [m (if (socket-option s 'rcvmore)
                  (receive-message* s as: 'blob)
                  '())])
      (fn t m)
      (receive-loop s)))

  (let ([s (make-socket 'sub)])
    (connect-socket s endpoint)
    (socket-option-set! s 'subscribe "")
    (receive-loop s)))

(define (demo)
  (subscribe "tcp://10.18.129.51:55555"
             (lambda (t m)
               (display (format "~a: ~a (~a bytes)~%"
                                (seconds->string (current-seconds))
                                t
                                (if (blob? m) (blob-size m) 0))))))
