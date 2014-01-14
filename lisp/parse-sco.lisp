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

(defun expect (expectations tokens &optional (ids '()))
  "Matches expectations to tokens. Returns three values: success, remaining tokens, variables."
  (labels ((variablep (thing)
             (and (symbolp thing)
                  (eq #\? (char (symbol-name thing) 0))))
           (var-name (var)
             (intern (subseq (symbol-name var) 1)))
           (iter (expectations tokens ids)
             (cond ((null expectations) (values t tokens ids)) ; all expectations met
                   ((null tokens) (values nil nil ids)) ; no more tokens, but still expectations
                   ((and (atom expectations)
                         (atom tokens))
                    (cond ((equalp expectations tokens) (values t '() ids)) ; two equal atoms
                          ((variablep expectations) ; variable matches any atom
                           (values t '() (cons (cons (var-name expectations)
                                                     tokens)
                                               ids)))
                          (t (values nil tokens ids)))) ; atoms don't match
                   ((and (consp expectations) ; match cons's recursively
                         (consp tokens))
                    (multiple-value-bind (success remaining-tokens ids) (iter (car expectations) (car tokens) ids)
                      (if (and success (null remaining-tokens))
                          (iter (cdr expectations) (cdr tokens) ids)
                          (values nil tokens ids)))) ; cons's don't match
                   (t (values nil tokens ids))))) ; atom and cons don't match
    (iter expectations tokens ids)))

