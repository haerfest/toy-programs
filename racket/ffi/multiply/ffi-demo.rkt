(require ffi/unsafe
         ffi/unsafe/define
         racket/path)

;;; Returns a list of directories in which to look for shared libraries.  You
;;; likely will need to change this.

(define (get-lib-dirs)
  (list
   (expand-user-path
    (string->path "~/Documents/Development/toy-programs/racket/ffi"))))

;;; Loads the multiply shared library, as in multiply.dylib on Mac OS X,
;;; libmultiply.so on other Unixes, or multiply.dll on Windows.

(define-ffi-definer define-multiply
  (ffi-lib "multiply" #f #:get-lib-dirs get-lib-dirs))

;;; The C code is: int multiply(int, int).

(define-multiply multiply (_fun _int _int -> _int))

;;; Test it.

(for-each (lambda (_)
            (let ((a (random 10))
                  (b (random 10)))
              (displayln (format "~a x ~a = ~a" a b (multiply a b)))))
          (range 10))

;;; Sample output:
;;
;; => #<void>
;;
;; 3 x 1 = 3
;; 9 x 6 = 54
;; 0 x 6 = 0
;; 7 x 6 = 42
;; 8 x 7 = 56
;; 9 x 3 = 27
;; 1 x 4 = 4
;; 5 x 2 = 10
;; 2 x 2 = 4
;; 2 x 9 = 18

