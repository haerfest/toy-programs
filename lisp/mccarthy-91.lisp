;;; http://www.reddit.com/r/dailyprogrammer/comments/1f7qp5/
;;;                              052813_challenge_127_easy_mccarthy_91_function/

(defun m (n)
  "Calculates the McCarthy 91 function of N (see Wikipedia) while outputting the
recursive steps it follows."
  (labels ((m (n ms)
             ;; Read MS as M's, the number of nested M(...)'s to output as an
             ;; indication of our recursion level.
             (multiple-value-bind (next-n ms descr fn)
                 ;; NEXT-N is the next value of N to calculate M for, MS is as
                 ;; above, DESCR tells us which path we took, and FN is a
                 ;; lambda expression that performs the next step when invoked.
                 (if (> n 100)
                     (values (- n 10)
                             ms
                             "greater"
                             (lambda () (- n 10)))
                     (values (+ n 11)
                             (+ ms 2)
                             "equal to or less"
                             (lambda ()
                               (m (m (+ n 11) (1+ ms))
                                  ms))))
               ;; Output where we are and what we are going to do next.
               (format t "~{~a~}~a~{~a~} since ~a is ~a than 100~%"
                       (loop repeat ms collect "M(")
                       next-n
                       (loop repeat ms collect ")")
                       n
                       descr)
               ;; Evaluate the next step.
               (funcall fn))))
    ;; Output where we start from and get going.
    (format t "M(~a)~%" n)
    (m n 0)))
