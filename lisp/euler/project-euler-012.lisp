;;; What is the value of the first triangle number to have over five hundred
;;; divisors?

(defun triangle-number (n)
  (/ (* n (+ n 1)) 2))

(defun divisors (n)
  (remove-duplicates
    (apply #'append
           (loop for d from 1 to (round (sqrt n))
                 when (zerop (rem n d))
                 collect (list d (/ n d))))))

(defun answer (min-divisors)
  (triangle-number
    (loop for n from 1
          until (>= (length (divisors (triangle-number n))) min-divisors)
          finally (return n))))

(answer 501)  ; => 76576500
