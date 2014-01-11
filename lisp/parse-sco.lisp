(ql:quickload 'cl-ppcre)                ; portable Perl-compatible regular expressions

(defun tokenize (stream)
  "Tokenizes the characters from a stream."
  (labels ((skip-to (chars &optional (skip-last nil) (skipped nil))
             (let ((c (read-char stream nil)))
               (cond ((null c) (nreverse skipped))
                     ((member c chars)
                      (unless skip-last
                        (unread-char c stream))
                      (nreverse skipped))
                     (t (skip-to chars skip-last (cons c skipped))))))
           (tok (tokens)
             (let ((c (read-char stream nil)))
               (case c
                 ((nil) (nreverse tokens))
                 ((#\space #\tab #\newline #\return) (tok tokens))
                 (#\# (skip-to '(#\newline #\return))
                      (tok tokens))
                 (#\{ (tok (cons 'block-begin tokens)))
                 (#\} (tok (cons 'block-end tokens)))
                 (#\= (tok (cons 'equals tokens)))
                 (#\" (tok (cons (cons 'value
                                       (coerce (skip-to '(#\") t) 'string))
                                 tokens)))
                 (t (tok (cons (cons 'identifier
                                     (coerce (cons c (skip-to '(#\space #\tab #\newline #\return #\# #\{ #\} #\=))) 'string))
                               tokens)))))))
    (tok '())))
