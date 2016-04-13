;; function composition in Scheme:
;;
;;   f(g(h(1, 2, 3)))
;; =
;;   f (g (h 1 2 3))
;; =
;;   (f . g . h) 1 2 3
;; =
;;   ((comp f g h) 1 2 3)

(define comp
  (lambda fs                            ; variable arguments
    (let ((f (first fs))                ; left-most function
          (gs (rest fs)))               ; remaining functions
      (if (empty? gs)                   ; base case: just the
          f                             ; ...one function f
          (let ((g (apply comp gs)))    ; recursive case: apply
            (lambda xs                  ; ...remaining functions
              (f (apply g xs))))))))    ; ...and finally f

(define (square x)
  (* x x))

(displayln ((comp square square) 2))    ; => 16

(define (mul x y)
  (* x y))

(displayln ((comp square mul) 2 3))     ; => 36
