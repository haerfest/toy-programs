(defun collatz-conjecture-length (n)
  (flet ((next (n)
               (if (evenp n)
                 (/ n 2)
                 (1+ (* 3 n)))))
    (loop for i = n then (next i)
          until (eq i 1)
          sum 1)))

(defun answer (upper-limit)
  (caar
    (sort
        (loop for n from 1 below upper-limit
              collect (cons n (collatz-conjecture-length n)))
        #'> :key #'cdr)))
