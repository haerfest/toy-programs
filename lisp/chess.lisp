(defparameter *board* '((BR BN BB BK BQ BB BN BR)
                        (BP BP BP BP BP BP BP BP)
                        (-- -- -- -- -- -- -- --)
                        (-- -- -- -- -- -- -- --)
                        (-- -- -- -- -- -- -- --)
                        (-- -- -- -- -- -- -- --)
                        (WP WP WP WP WP WP WP WP)
                        (WR WN WB WK WQ WB WN WR)))

(defun print-board (&optional (board *board*))
  (format t "~{~{~A ~}~%~}" board))
  
(defun read-move ()
  (format t "? ")
  (let ((move (read-line)))
    (if (= 4 (length move))
        (let ((from-col (char-upcase (char move 0)))
              (from-row (- (char-code (char move 1)) 48))
              (to-col   (char-upcase (char move 2)))
              (to-row   (- (char-code (char move 3)) 48)))
          (if (and (char<= #\A from-col #\H)
                   (char<= #\A to-col   #\H)
                   (<= 1 from-row 8)
                   (<= 1 to-row   8)
                   (not (and (eq from-row to-row)
                             (eq from-col to-col))))
              (list from-col from-row to-col to-row)
              (read-move)))
        (read-move))))
             
             
       
  
