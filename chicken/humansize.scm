(use format)

(define suffixes '((1000 . (KB MB GB TB PB EB ZB YB))
                   (1024 . (KiB MiB GiB TiB PiB EiB ZiB YiB))))

(define (approximate-size size #!optional (a-kilobyte-is-1024-bytes #t))
  (if (< size 0)
      (signal "number must be non-negative")
      (letrec ((multiple (if a-kilobyte-is-1024-bytes 1024 1000))
               (loop (lambda (size suffixes)
                       (if (null? suffixes)
                           (signal "number too large")
                           (let ((size (/ size multiple)))
                             (if (< size multiple)
                                 (format #f "~,1f ~a" size (car suffixes))
                                 (loop size (cdr suffixes))))))))
        (loop size (cdr (assq multiple suffixes))))))
                           
(display (approximate-size 1000000000000 #f))
(newline)
(display (approximate-size 1000000000000))
(newline)
