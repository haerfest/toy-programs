;; http://www.ulisp.com/show?1BFA

;; X is a unique color mixture number in the range [0, 767].
;; 
;; Its red, green and blue components are represented by
;; three partially overlapping pyramid shapes of length 512,
;; at different intervals in the range:
;;
;; value | red | grn | blu | appearance
;; ------|-----|-----|-----|-----------
;;     0 |   0 | 255 |   0 | green
;;   127 | 127 | 128 |   0 | green/red
;;   255 | 255 |   0 |   0 | red
;;   383 | 128 |   0 | 127 | red/blue
;;   511 |   0 |   0 | 255 | blue
;;   639 |   0 | 127 | 128 | blue/green

(defun red (x)
  (let ((y (mod x 768)))
    (max
     (if (> y 255) (- 511 y) y)
     0)))

(defun grn (x) (red (+ x 256)))
(defun blu (x) (red (+ x 512)))

(defun rgb (v)
  (mapc analogwrite
        '(9 10 11)
        (list (red v) (grn v) (blu v))))

(defun run ()
  (let ((x 0))
    (loop
       (rgb x)
       (setq x (mod (1+ x) 768))
       (delay 10))))
