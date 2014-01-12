(ql:quickload 'cl-ppcre)                ; portable Perl-compatible regular expressions

(defun tokenize (stream)
  "Returns a list with the tokens read from a stream containing a specification
in the SCO format."
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
                                     (coerce (cons c
                                                   (skip-to '(#\space #\tab #\newline #\return #\# #\{ #\} #\=)))
                                             'string))
                               tokens)))))))
    (tok '())))

(defun expect (expectations tokens)
  "Matches tokens to expectations.  Throws an error upon failure to match.
Upon a successful match, returns an ALIST of matching variables, or NIL if no
variables were specified.

Examples:

 (expect 'foo 'foo) => NIL
 (expect '?x 'foo) => ((X . FOO))
 (expect '(name ?name age ?age) '(name \"John\" age \"25\"))
 => ((AGE . 25) (NAME . \"John\"))
  (expect 'foo 'bar) => ERROR"
  (labels ((iter (expectations tokens ids)
             (cond ((null expectations) ids)
                   ((null tokens) (error 'no-more-tokens))
                   ((and (atom expectations)
                         (atom tokens))
                    ;; two atoms are equal or the expectation is a variable
                    (cond ((equal expectations tokens) ids)
                          ((and (symbolp expectations)
                                (eq #\? (char (symbol-name expectations) 0)))
                           (cons (cons (intern (subseq (symbol-name expectations) 1))
                                       tokens)
                                 ids))
                          (t (error 'unmet-atomic-expectation))))
                   ((and (listp expectations)
                         (listp tokens))
                    (iter (cdr expectations)
                          (cdr tokens)
                          (iter (car expectations) (car tokens) ids)))
                   (t (error 'cannot-match-atoms-to-lists)))))
    (iter expectations tokens '())))

(defparameter *sco*
  (format nil "foo~%{~%  bar = \"rab\"  # comment~%  baz = \"zab\"~%}")
  "Example SCO input:

   foo
   {
     bar = \"rab\"  # comment
     baz = \"zab\"
   }")

(defparameter *expectation*
  '((IDENTIFIER . "foo")
    BLOCK-BEGIN
    (IDENTIFIER . ?a) EQUALS (VALUE . ?a-value)
    (IDENTIFIER . ?b) EQUALS (VALUE . ?b-value)
    BLOCK-END))

(defun demo ()
  "Returns ((B-VALUE . \"zab\") (B . \"baz\") (A-VALUE . \"rab\") (A . \"bar\"))"
  (with-input-from-string (stream *sco*)
    (expect *expectation* (tokenize stream))))
