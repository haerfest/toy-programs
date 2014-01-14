(ql:quickload 'cl-ppcre)                ; portable Perl-compatible regular expressions

(defun tokenize (stream)
  "Returns a list of SCO tokens, as read from a stream."
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
                 ((nil) (nreverse tokens)) ; reached end of stream
                 ((#\space #\tab #\newline #\return) (tok tokens)) ; skip whitespace and line endings
                 (#\# (skip-to '(#\newline #\return)) ; skip comment to end of line
                      (tok tokens))
                 (#\{ (tok (cons 'begin tokens)))
                 (#\} (tok (cons 'end tokens)))
                 (#\= (tok (cons 'equals tokens)))
                 (#\" (tok (cons (cons 'val
                                       (coerce (skip-to '(#\") t) 'string))
                                 tokens))) ; read string until next double quote
                 (t (tok (cons (cons 'id
                                     (coerce (cons c
                                                   (skip-to '(#\space #\tab #\newline #\return #\# #\{ #\} #\=)))
                                             'string))
                               tokens))))))) ; read identifier until whitepsace, line endings or other tokens
    (tok '())))

(defun lookahead (expectations tokens)
  (labels ((variablep (thing)
             (and (symbolp thing) (eq #\? (char (symbol-name thing) 0))))
           (var-name (var)
             (intern (subseq (symbol-name var) 1)))
           (iter (expectations tokens ids)
             (cond ((null expectations) ; all expectations met
                    (if (null ids) t ids))
                   ((null tokens) nil) ; no more tokens, but still expectations
                   ((and (atom expectations)
                         (atom tokens))
                    (cond ((variablep expectations) ; variable matches any atom
                           (cons (cons (var-name expectations)
                                       tokens)
                                 ids))
                          ((equalp expectations tokens) ; two equal atoms
                           (if (null ids) t ids))))
                   ((and (consp expectations) ; match cons's recursively
                         (consp tokens))
                    (let ((ids (iter (car expectations) (car tokens) ids)))
                      (if (null ids)
                          nil
                          (iter (cdr expectations)
                                (cdr tokens)
                                (if (consp ids) ids '()))))))))
    (iter expectations tokens '())))
