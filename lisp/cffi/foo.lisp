(ql:quickload "cffi")

(defpackage :foo
  (:use :common-lisp :cffi))

(in-package :foo)

(define-foreign-library libfoo
  (t (:default "libfoo")))

;;; You may have to set the library path first:
;;; (pushnew #P"..." *foreign-library-directories* :test #'equal)
(use-foreign-library libfoo)

(defcfun "add_two_ints" :int
  (a :int)
  (b :int)
  (c :pointer))

(defun demo ()
  "Calls the add_two_ints() function in the external C libfoo library, to sum 1
and 2.  Returns (0 3), where 0 is the return code and 3 is the sum."
  (with-foreign-object (pointer :int 1)
    (let* ((rc (add-two-ints 1 2 pointer))
           (sum (mem-ref pointer :int)))
      (list rc sum))))
