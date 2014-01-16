(ql:quickload 'cl-ppcre)                ; portable Perl-compatible regular expressions

(defun tokenize (stream)
  (labels ((skip-to (chars &optional (skip-over nil) (skipped nil))
             (let ((c (read-char stream nil)))
               (cond
                 ;; end of stream
                 ((null c) (nreverse skipped))
                 ;; skip to or over this char
                 ((member c chars) (unless skip-over
                                     (unread-char c stream))
                                   (nreverse skipped))
                 ;; skip over this char
                 (t (skip-to chars skip-over (cons c skipped))))))
           (tok (tokens)
             (let ((c (read-char stream nil)))
               (case c
                 ;; end of stream
                 ((nil) (nreverse tokens))
                 ;; skip whitespace and line ends
                 ((#\space #\tab #\newline #\return) (tok tokens))
                 ;; skip comment to line end
                 (#\# (skip-to '(#\newline #\return))
                      (tok tokens))
                 ;; special tokens
                 (#\{ (tok (cons 'begin tokens)))
                 (#\} (tok (cons 'end tokens)))
                 (#\= (tok (cons 'equals tokens)))
                 ;; read string until next "
                 (#\" (tok (cons (cons 'val
                                       (coerce (skip-to '(#\") t) 'string))
                                 tokens)))
                 ;; read identifier until whitespace, line end, or token
                 (t (tok (cons (cons 'id
                                     (coerce (cons c
                                                   (skip-to '(#\space #\tab #\newline #\return #\# #\{ #\} #\=)))
                                             'string))
                               tokens)))))))
    (tok '())))

(defun lookahead (expectations tokens)
  (labels ((varp (thing)
             (and (symbolp thing)
                  (eq #\? (char (symbol-name thing) 0))))
           (var-name (var)
             (intern (subseq (symbol-name var) 1)))
           (iter (expectations tokens ids)
             (cond
               ;; all expectations met
               ((null expectations) (if (null ids)
                                        t
                                        ids))
               ;; no more tokens, but still expectations
               ((null tokens) nil)
               ;; two atoms
               ((and (atom expectations)
                     (atom tokens))
                (cond
                  ;; a variable matches any atom
                  ((varp expectations) (cons (cons (var-name expectations)
                                                   tokens)
                                             ids))
                  ;; two equal atoms
                  ((equalp expectations tokens) (if (null ids)
                                                    t
                                                    ids))))
               ;; two conses: match recursively
               ((and (consp expectations)
                     (consp tokens))
                (let ((ids (iter (car expectations) (car tokens) ids)))
                  (if (null ids)
                      nil
                      (iter (cdr expectations) (cdr tokens) (if (consp ids)
                                                                ids
                                                                '()))))))))
    (iter expectations tokens '())))
