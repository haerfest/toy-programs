(define (cat filename)
  (define (cat-port port)
    (let ((char (read-char port)))
      (unless (eof-object? char)
              (write-char char)
              (cat-port port))))
  (call-with-input-file filename cat-port))

(define (display-hex x width)
  (let* ((str (number->string x 16))
         (k   (max 0 (- width (string-length str))))
         (pad (make-string k #\0)))
    (display pad)
    (display str)))

(define (ascii-dump chars width)
  (unless (null? chars)
          (let ((k (* 3 (- width (length chars)))))
            (display (make-string k #\space)))
          (display "| ")
          (display (list->string
                    (map (lambda (char)
                           (if (or (char<? char #\space) (char>? char #\~))
                               #\.
                               char))
                         (reverse chars))))
          (newline)))

(define (hexdump filename)
  (call-with-input-file filename
    (lambda (port)
      (define (iterate width count chars)
        (let ((char (read-char port)))
          (if (eof-object? char)
              (ascii-dump chars width)
              (let ((new-line? (zero? (remainder count width))))
                (when new-line?
                      (ascii-dump chars width)
                      (display-hex count 8)
                      (display " | "))
                (display-hex (char->integer char) 2)
                (display " ")
                (iterate width
                         (add1 count)
                         (if new-line? (list char) (cons char chars)))))))
      (iterate 16 0 '()))))
