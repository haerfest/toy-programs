;; Computes the root of a function using the Newton-Raphson method.

(defun newton (f initial-guess)
  (labels ((f-derivative (x)
             (let ((dx 0.001))
               (/ (funcall f x) dx)))
           (check (guess)
             (let ((diff (abs (funcall f guess)))
                   (eps 0.01))
               (if (< diff eps)
                   guess
                   (let ((improved-guess (+ guess (/ (funcall f guess)
                                                     (f-derivative guess)))))
                     (check improved-guess))))))
    (check initial-guess)))

(defun newton-sqrt (n)
  (newton (lambda (x) (- (* x x) n)) 1.0))

;; Example session:
;; > (newton-sqrt 100.0)
;; 10.000421
