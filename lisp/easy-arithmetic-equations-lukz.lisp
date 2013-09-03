;;; http://www.reddit.com/r/dailyprogrammer/comments/1k7s7p/
;;;                              081313_challenge_135_easy_arithmetic_equations/
;;; 
;;; This solution by http://www.reddit.com/user/lukz, analyzed by me to figure
;;; out what is going on :)

(defvar *e* "Will hold the random equation.")

(defun mult ()
  "Multiplies all a * b * c * ... terms at the head of *e*, removing them from
  *e*.  Returns the result of the multiplication."
  (do ((a (pop *e*)                     ; loop: take first number...
          (* a (pop *e*))))             ; ...multiply w/ second number
      ((not (eq '* (car *e*))) a)       ; return answer so far if not *
    (pop *e*)))                         ; body: remove *

(defun evaluate (*e*)
  "Evaluates all terms in *e*, emptying *e* and returning the result."
  (do ((a (mult)                        ; try to multiply first terms
          (if (eq '+ (pop *e*))         ; next op must be + or -
              (+ a (mult))              ; add next multiplication/term
              (- a (mult)))))           ; subtract next multiplication/term
      ((not *e*) a)))                   ; return answer when *e* is consumed

(defun generate (lo hi &aux r)                         ; variable r is nil
  "Creates a list of four random numbers in the range [lo, hi], infixed by
  three operators chosen randomly from (+ - *)."
  (dotimes (i 4                                        ; add 4 times
            (cdr r))                                   ; return: strip last op
    (push (+ lo (random (- hi lo -1))) r)              ; add random number
    (push (nth (random 3) '(+ - *)) r)))               ; add random operator

(defun give-problem (p &aux (e (evaluate p)))          ; e contains answer
  "Repeatedly prints a random equation and reads in the user's answer.  Repeats
  the same equation until the user gets it right.  Returns with nil when the
  user inputs q, t when the user answered correctly."
  (do (a)                                              ; variable a
      ((progn (setf a (read (format t "~{~a ~}~%" p))) ; stop condition is...
              (eq a 'q)))                              ; ...(eq a 'q)
    (when (= a e) (format t "Correct~%") (return t))   ; body...
    (format t "Incorrect~%")))                         ; ... continued

(defun main (&aux (lo (read)) (hi (read)))             ; read in lo and hi
  "Reads a lower adn upper bound (inclusive) for generating random numbers and
  presents the user with random equations to solve."
  (do ()                                               ; loop until...
      ((not (give-problem (generate lo hi))))))        ; ...t returned
