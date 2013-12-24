(require-extension zmq)
(require-extension posix)

(define (subscribe endpoint fn)
  (let ([s (make-socket 'sub)])
    (connect-socket s endpoint)
    (socket-option-set! s 'subscribe "")

    (define (receive-loop)
      (let* ([t (receive-message* s as: 'string)]
             [m (if (socket-option s 'rcvmore)
                    (receive-message* s as: 'blob)
                    '())])
        (fn t m)
        (receive-loop)))

    (receive-loop)))

(define (demo)
  (subscribe "tcp://10.18.129.51:55555"
             (lambda (t m)
               (display (format "~a: ~a (~a bytes)~%"
                                (seconds->string (current-seconds))
                                t
                                (if (blob? m) (blob-size m) #\?))))))
