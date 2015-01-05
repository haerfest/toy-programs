(defun answer (size)
  (when (oddp size)
    (labels ((diag (i)
               (loop
                  for n from 3 to size by 2
                  sum (+ (* n n) (* i (- 1 n))))))
      (let ((tr (diag 0))               ; top-right diagonal
            (tl (diag 1))               ; top-left diagonal
            (bl (diag 2))               ; bottom-left diagonal
            (br (diag 3)))              ; bottom-right diagonal
        (+ 1 br bl tl tr)))))
