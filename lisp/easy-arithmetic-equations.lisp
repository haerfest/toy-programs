;;; http://www.reddit.com/r/dailyprogrammer/comments/1k7s7p/
;;;                              081313_challenge_135_easy_arithmetic_equations/

(setf *random-state* (make-random-state t))

(defun random-op ()
  (let ((ops '(+ - *)))
    (nth (random (length ops)) ops)))

(defun random-num (min max)
  (+ min (random (- max min -1))))

(defun expr (tokens)
  "EBNF: expr := term {+/- term}"
  (labels ((f (parent-form remaining)
             ;; parent-form is incomplete and something like (+ 1)
             (multiple-value-bind (form remaining) (term remaining)
               (let ((op (car remaining)))
                 (let ((filled-parent-form
                        (if (null parent-form)
                            form
                            ;; change (+ 1) and 2 into (+ 1 2)
                            (append parent-form (list form)))))
                   (if (member op '(+ -))
                       (multiple-value-bind (sub-form remaining)
                           (f (list op filled-parent-form) (cdr remaining))
                         (values sub-form remaining))
                       (values filled-parent-form remaining)))))))
    (f nil tokens)))

(defun term (tokens)
  "EBNF: term := num {* num}"
  (if (null tokens)
      nil
      (labels ((f (numbers remaining)
                 (let ((numbers (cons (car remaining) numbers))
                       (op      (cadr remaining)))
                   (if (eq op '*)
                       ;; collect more nums
                       (f numbers (cddr remaining)) 
                       ;; return a or (* a b c ...)
                       (if (> (length numbers) 1)
                           (values (cons '* (reverse numbers)) (cdr remaining))
                           (values (car numbers) (cdr remaining)))))))
        (f nil tokens))))

(defun make-equation (min max)
  (list (random-num min max)
        (random-op)
        (random-num min max)
        (random-op)
        (random-num min max)
        (random-op)
        (random-num min max)))

(defun start ()
  (let ((min (read))                    ; read min/max (inclusive) number range
        (max (read)))
    (loop named playing
       do (let* ((tokens (make-equation min max))
                 (ans    (eval (expr tokens))))
            (loop named answering
               do (progn
                    (format t "~{~a ~}~%" tokens)
                    (let ((input (read-line)))
                      (cond ((equal input "q") (return-from playing))
                            ((equal (parse-integer input :junk-allowed t) ans)
                             (format t "Correct!~%")
                             (return-from answering))
                            (t (format t "Incorrect...~%"))))))))))
