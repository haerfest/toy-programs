(defun hanoi (n &optional (from 1) (to 3) (spare 2))
  "Towers of Hanoi."
  (if (= n 1)
      (format t "Move disc from ~A to ~A.~%" from to)
      (progn
       (hanoi (- n 1) from spare to)
       (hanoi 1 from to spare)
       (hanoi (- n 1) spare to from))))
