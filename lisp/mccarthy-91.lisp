;;; http://www.reddit.com/r/dailyprogrammer/comments/1f7qp5/
;;;                              052813_challenge_127_easy_mccarthy_91_function/

(defun m (n)
  (labels ((iter (n level)
             (multiple-value-bind (next ms descr fn)
                 (if (> n 100)
                     (values (- n 10)
                             level
                             "greater"
                             (lambda () (- n 10)))
                     (values (+ n 11)
                             (+ level 2)
                             "equal to or less"
                             (lambda ()
                               (iter (iter (+ n 11) (1+ level))
                                     level))))
               (format t "~{~a~}~a~{~a~} since ~a is ~a than 100~%"
                       (loop repeat ms collect "M(")
                       next
                       (loop repeat ms collect ")")
                       n
                       descr)
               (funcall fn))))
    (format t "M(~a)~%" n)
    (iter n 0)))
