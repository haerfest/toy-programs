(ql:quickload 'cl-ppcre)                ; portable Perl-compatible regular expressions

(defun tokenize (stream)
  (labels ((skip-to (chars &optional (skipped nil))
             (let ((c (read-char stream nil)))
               (cond ((null c) skipped)
                     ((member c chars) (unread-char c stream) skipped)
                     (t (skip-to chars (append skipped (list c)))))))
           (tok (tokens)
             (let ((c (read-char stream nil)))
               (case c
                 ((nil) tokens)
                 ((#\space #\tab #\newline #\return) (tok tokens))
                 (#\# (skip-to '(#\newline #\return))
                      (tok tokens))
                 (#\{ (tok (append tokens '(open-bracket))))
                 (#\} (tok (append tokens '(close-bracket))))
                 (#\= (tok (append tokens '(equals))))
                 (#\" (tok (append tokens (list (cons 'value
                                                      (skip-to '(#\")))))))
                 (t (tok (append tokens (list (cons 'identifier
                                                    (cons c (skip-to '(#\space #\tab #\newline #\return #\# #\=))))))))))))
    (tok '())))

(defparameter *regexp* "([^\"\\s]+)\\s*=\\s*\"([^\"]*)\""
  "Matches a line of <id> = <value>")

(define-condition unexpected-input-error (error)
  ((regexp :initarg :regexp :reader regexp)
   (input  :initarg :input  :reader input)))

(defun read-strip (stream)
  (string-trim '(#\space #\tab #\newline #\return) (read-line stream nil)))

(defun expect (regexp stream)
  (let ((line (read-strip stream)))
    (multiple-value-bind (_ matches) (cl-ppcre:scan-to-strings regexp line)
      (if (null matches)
          (error 'unexpected-input-error :regexp regexp :input line)
          (coerce matches 'list)))))

(defun read-block (stream)
  (let ((id              (car (expect "^\\s*([A-Za-z0-9]+)" stream)))
        (opening-bracket (car (expect "^\\s*(\\{)"          stream))))
    (list id opening-bracket)))

(defun read-assignment (stream)
  )

(defun read-sco (filename)
  (with-open-file (stream filename)
    (loop
       for block = (read-block stream)
       until (null block)
       do (format "Read a block~%"))))

(defun parse-sco2 (filename)
  (with-open-file (stream filename)
    (loop
       for line = (read-line stream nil)
       until (null line)
       ;; trim spaces and line endings
       for trimmed = (string-trim '(#\space #\tab #\newline #\return) line)
       ;; skip empty lines and comment lines
       when (not (or (zerop (length trimmed))
                     (eq (elt trimmed 0) #\#)))
       collect (multiple-value-bind (_ matches) (cl-ppcre:scan-to-strings *regexp* trimmed)
                 (when matches
                   (cons (elt matches 0) (elt matches 1)))))))
              