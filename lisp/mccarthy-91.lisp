;;; http://www.reddit.com/r/dailyprogrammer/comments/1f7qp5/
;;;                              052813_challenge_127_easy_mccarthy_91_function/

(defun m (n)
  (labels ((iter (n level)
             (let ((next (if (> n 100)
                             (- n 10)
                             (+ n 11)))
                   (ms (if (> n 100)
                           level
                           (+ level 2))))
               (format t "~{~a~}~a~{~a~} since ~a is ~a than 100~%"
                       (loop repeat ms collect "M(")
                       next
                       (loop repeat ms collect ")")
                       n
                       (if (> n 100)
                           "greater"
                           "equal to or less"))
               (if (> n 100)
                   next
                   (iter (iter next (1+ level))
                         level)))))
    (format t "M(~a)~%" n)
    (iter n 0)))
