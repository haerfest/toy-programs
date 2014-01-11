(ql:quickload 'cl-ppcre)                ; portable Perl-compatible regular expressions

(defun tokenize (stream)
  "Tokenizes the characters from a stream.  Example usage:
(with-input-from-string (s \"target{me=\\\"you\\\"}\") (tokenize s))
gives:
((IDENTIFIER . \"target\") OPEN-BRACKET (IDENTIFIER . \"me\") EQUALS (VALUE . \"you\") CLOSE-BRACKET)"
  (labels ((skip-to (chars &optional (skip-char nil) (skipped nil))
             (let ((c (read-char stream nil)))
               (cond ((null c) skipped)
                     ((member c chars)
                      (unless skip-char
                        (unread-char c stream))
                      skipped)
                     (t (skip-to chars skip-char (append skipped (list c)))))))
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
                                                      (coerce (skip-to '(#\") t) 'string))))))
                 (t (tok (append tokens (list (cons 'identifier
                                                    (coerce (cons c (skip-to '(#\space #\tab #\newline #\return #\# #\{ #\} #\=))) 'string))))))))))
    (tok '())))
