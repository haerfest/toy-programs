(defparameter *board*
  (make-array '(8 8)
              :initial-contents '(((R . B) (N . B) (B . B) (K . B) (Q . B) (B . B) (N . B) (R . B))
                                  ((P . B) (P . B) (P . B) (P . B) (P . B) (P . B) (P . B) (P . B))
                                  (NONE    NONE    NONE    NONE    NONE    NONE    NONE    NONE)
                                  (NONE    NONE    NONE    NONE    NONE    NONE    NONE    NONE)
                                  (NONE    NONE    NONE    NONE    NONE    NONE    NONE    NONE)
                                  (NONE    NONE    NONE    NONE    NONE    NONE    NONE    NONE)
                                  ((P . W) (P . W) (P . W) (P . W) (P . W) (P . W) (P . W) (P . W))
                                  ((R . W) (N . W) (B . W) (K . W) (Q . W) (B . W) (N . W) (R . W)))))

(defmacro square (pos board)
  `(aref ,board (- 8 (cdr ,pos)) (1- (car ,pos))))

(defun empty-p (piece)
  (eq piece 'NONE))

(defun occupied-p (piece)
  (not (empty-p piece)))

(defun kind (piece)
  (car piece))

(defun color (piece)
  (cdr piece))

(defun print-board (board)
  (dotimes (row 8)
    (format t "~A " (- 8 row))
    (dotimes (col 8)
      (let ((piece (aref board row col)))
        (if (occupied-p piece)
            (format t "~A~A " (color piece) (kind piece))
            (format t " . "))))
    (format t "~%"))
  (format t "   A  B  C  D  E  F  G  H~%"))

(defun valid-pos-p (pos)
  (let ((col (car pos))
        (row (cdr pos)))
    (and (<= 1 col 8)
         (<= 1 row 8))))

(defun read-move ()
  (format t "? ")
  (let ((move (read-line)))
    (if (= 4 (length move))
        (let ((from (cons (- (char-code (char-upcase (char move 0))) 64)
                          (- (char-code              (char move 1))  48)))
              (to   (cons (- (char-code (char-upcase (char move 2))) 64)
                          (- (char-code              (char move 3))  48))))
          (if (and (valid-pos-p from)
                   (valid-pos-p to)
                   (not (equal from to)))
              (cons from to)
              (read-move)))
        (read-move))))

(defun make-move (move board)
  (let* ((from (car move))
         (to   (cdr move))
         (piece (square from board)))
    (setf (square from board) 'NONE)
    (setf (square to   board) piece)))

(defun valid-pawn-move-p (from to board)
  (let ((from-col (car from))
        (from-row (cdr from))
        (to-col   (car to))
        (to-row   (cdr to)))
    (or
     ;; two squares forward from starting position
     (and (eq to-col from-col)
          (eq from-row 2)
          (eq to-row 4)
          (empty-p (square to board))
          (empty-p (square (cons to-col 3) board)))
     ;; one square forward
     (and (eq to-col from-col)
          (eq to-row (1+ from-row))
          (empty-p (square to board)))
     ;; one step diagonally, taking a black piece
     (and (or (eq to-col (1- from-col))
              (eq to-col (1+ from-col)))
          (eq to-row (1+ from-row))
          (occupied-p (square to board))
          (eq (color (square to board)) 'B)))))

(defun valid-move-p (move board)
  (let* ((from  (car move))
         (to    (cdr move)))
    (and (not (empty-p (square from board)))
         (case (kind (square from board))
           ('P (valid-pawn-move-p from to board))))))
    
(defun test ()
  (print-board *board*)
  (labels ((read-valid-move ()
             (let ((move (read-move)))
               (if (valid-move-p move *board*)
                   move
                   (read-valid-move)))))
    (make-move (read-valid-move) *board*)
    (test)))
