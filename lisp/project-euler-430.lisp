(defun e (n m)
   (labels 
     ((turn-probability (disk)
       (/ (1- (* 2 disk (1+ (- n disk))))
          n
          n))

      (expected-value (disk)
        (let ((p (turn-probability disk)))
          (loop repeat (1+ m)
                for w = 1.0 then (+ (* w (- 1 p))
                                    (* (- 1 w) p))
                finally (return w)))))

     (let ((middle-disk (truncate n 2)))
       (loop for disk from 1 to middle-disk
             summing (expected-value disk) into sum
             finally (return (+ (* 2 sum)
                                (if (oddp n)
                                  (expected-value (1+ middle-disk))
                                  0)))
             do (format t "~C~a" #\return disk)))))
