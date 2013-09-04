;;; 1             => 1
;;; '(1 2 3)      => (1 2 3)
;;; '(1 2 . 3)    => (1 2 . 3)
;;; '(1 2 . (3))  => (1 2 3)
;;; NIL           => NIL
;;; '()           => NIL
;;; '(1 () 3)     => (1 NIL 3)
;;; '(1 2 . NIL)  => (1 2)
;;; '(1 2 . ())   => (1 2)
;;; '(1 2 . '())  => (1 2 QUOTE NIL)

(defun print-expr (expr)
  (cond ((atom expr)
         (princ expr))
        (t
         (princ "(")
         (print-cons expr)
         (princ ")")))
  nil)

(defun print-cons (expr)
  (unless (null expr)
    (cond
      ((listp (rest expr))
       (print-expr (first expr))
       (unless (null (rest expr))
         (princ " ")
         (print-cons (rest expr))))
      (t
        (print-expr (first expr))
        (princ " . ")
        (princ (rest expr))))))
