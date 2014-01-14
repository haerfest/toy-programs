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
                 (#\{ (tok (cons 'block-begin tokens)))
                 (#\} (tok (cons 'block-end tokens)))
                 (#\= (tok (cons 'equals tokens)))
                 (#\" (tok (cons (cons 'value
                                       (coerce (skip-to '(#\") t) 'string))
                                 tokens))) ; read string until next double quote
                 (t (tok (cons (cons 'identifier
                                     (coerce (cons c
                                                   (skip-to '(#\space #\tab #\newline #\return #\# #\{ #\} #\=)))
                                             'string))
                               tokens))))))) ; read identifier until whitepsace, line endings or other tokens
    (tok '())))

(defun expect (expectations tokens)
  "Matches (a list of) expectations to (a list of) SCO tokens.  An expectation
can be a ?VARIABLE, which, when matched, will be returned as an ALIST of
(VARIABLE . value) pairs.  Returns two values, the first indicating success, the
second is a possible ALIST of variable matches."
  (labels ((iter (expectations tokens ids)
             (cond ((null expectations) ids) ; all expectations met
                   ((null tokens) 'fail)     ; no more tokens, but still expectations
                   ((and (atom expectations)
                         (atom tokens))
                    (cond ((equal expectations tokens) ids) ; two equal atoms
                          ((and (symbolp expectations)      ; we expect a ?variable
                                (eq #\? (char (symbol-name expectations) 0)))
                           (cons (cons (intern (subseq (symbol-name expectations) 1))
                                       tokens)
                                 ids))
                          (t 'fail)))   ; atoms don't match
                   ((and (consp expectations) ; match cons's recursively
                         (consp tokens))
                    (iter (cdr expectations)
                          (cdr tokens)
                          (iter (car expectations) (car tokens) ids)))
                   (t 'fail))))         ; atom and cons don't match
    (let ((result (iter expectations tokens '())))
      (if (eq result 'fail)
          (values nil nil)
          (values t result)))))

(defun demo ()
  (let* ((sco          (format nil "foo~%{~%  bar = \"b.a.r.\"  # comment~%  baz~%  {~%    bax = \"b.a.x.\"  }~%}~%"))
         (tokens       (with-input-from-string (stream sco)
                         (tokenize stream)))
         (expectations '((identifier . ?foo)
                         block-begin
                         (identifier . ?bar) equals (value . ?bar-val)
                         (identifier . ?baz)
                         block-begin
                         (identifier . ?bax) equals (value . ?bax-val)
                         block-end
                         block-end)))
    (princ tokens)
    (expect expectations tokens)))