(defparameter *debug* t)

(defparameter *initial-board*
  '(((R . B) (N . B) (B . B) (K . B) (Q . B) (B . B) (N . B) (R . B))
    ((P . B) (P . B) (P . B) (P . B) (P . B) (P . B) (P . B) (P . B))
    (NONE    NONE    NONE    NONE    NONE    NONE    NONE    NONE)
    (NONE    NONE    NONE    NONE    NONE    NONE    NONE    NONE)
    (NONE    NONE    NONE    NONE    NONE    NONE    NONE    NONE)
    (NONE    NONE    NONE    NONE    NONE    NONE    NONE    NONE)
    ((P . W) (P . W) (P . W) (P . W) (P . W) (P . W) (P . W) (P . W))
    ((R . W) (N . W) (B . W) (K . W) (Q . W) (B . W) (N . W) (R . W))))

(defparameter *scores* '((P . 3) (R . 5) (N . 7) (B . 5) (Q . 10) (K . 100)))

(defun col (pos)
  (car pos))

(defun row (pos)
  (cdr pos))

(defun from (move)
  (car move))

(defun to (move)
  (cdr move))

(defmacro square (pos board)
  `(aref ,board (- 7 (row ,pos)) (col ,pos)))

(defmacro each-square (row col &body body)
  `(dotimes (,row 8)
     (dotimes (,col 8)
       ,@body)))

(defun empty-p (pos board)
  (eq (square pos board) 'NONE))

(defun occupied-p (pos board)
  (not (empty-p pos board)))

(defun kind (pos board)
  (car (square pos board)))

(defun color (pos board)
  (cdr (square pos board)))

(defun opponent-color (color)
  (if (eq color 'B) 'W 'B))
  
(defun print-board (board)
  (dotimes (row 8)
    (format t "~A " (- 8 row))
    (dotimes (col 8)
      (let ((pos (cons col (- 7 row))))
        (if (occupied-p pos board)
            (format t "~A~A " (color pos board) (kind pos board))
            (format t " . "))))
    (format t "~%"))
  (format t "   A  B  C  D  E  F  G  H~%"))

(defun valid-pos-p (pos)
    (and (<= 0 (col pos) 7)
         (<= 0 (row pos) 7)))

(defun read-move ()
  (format t "? ")
  (let ((move (read-line)))
    (if (= 4 (length move))
        (let ((from (cons (- (char-code (char-upcase (char move 0))) 65)
                          (- (char-code              (char move 1))  49)))
              (to   (cons (- (char-code (char-upcase (char move 2))) 65)
                          (- (char-code              (char move 3))  49))))
          (if (and (valid-pos-p from)
                   (valid-pos-p to))
              (cons from to)
              (read-move)))
        (read-move))))

(defun make-move (move board)
  (setf (square (to   move) board) (square (from move) board)
        (square (from move) board) 'NONE))

(defun line-of-sight-p (from to board Δcol Δrow)
  (do ((pos (cons (+ (col from) Δcol) (+ (row from) Δrow))
            (cons (+ (col pos)  Δcol) (+ (row pos)  Δrow))))
      ((or (equal pos to)
           (occupied-p pos board))
       (equal pos to))))

(defun valid-pawn-move-p (from to board color)
  (let ((start (if (eq color 'W) 1 6)))
    (flet ((next (row) (if (eq color 'W) (1+ row) (1- row))))
      (or
       ;; two squares forward from starting position
       (and (= (col to) (col from))
            (= (row from) start)
            (= (row to) (next (next (row from))))
            (empty-p to board)
            (empty-p (cons (col from) (next (row from))) board))
       ;; one square forward
       (and (= (col to) (col from))
            (= (row to) (next (row from)))
            (empty-p to board))
       ;; one step diagonally, taking the opponent's piece
       (and (= (row to) (next (row from)))
            (= 1 (abs (- (col to) (col from))))
            (occupied-p to board)
            (eq (color to board) (opponent-color color)))))))

(defun valid-rook-move-p (from to board color)
  (let ((Δcol (- (col to) (col from)))
        (Δrow (- (row to) (row from))))
    (and (or (= 1 (abs Δcol))
             (= 1 (abs Δrow)))
         (or (= 0 Δcol)
             (= 0 Δrow))
         (line-of-sight-p from to board Δcol Δrow)
         (or (empty-p to board)
             (eq (color to board) (opponent-color color))))))

(defun valid-bishop-move-p (from to board color)
  (let ((Δcol (- (col to) (col from)))
        (Δrow (- (row to) (row from))))
    (and (= 1 (abs Δrow))
         (= 1 (abs Δcol))
         (line-of-sight-p from to board Δcol Δrow)
         (or (empty-p to board)
             (eq (color to board) (opponent-color color))))))

(defun valid-knight-move-p (from to board color)
  (let ((Δrow (abs (- (row to) (row from))))
        (Δcol (abs (- (col to) (col from)))))
    (and (or (and (= 1 Δrow)
                  (= 2 Δcol))
             (and (= 2 Δrow)
                  (= 1 Δcol)))
         (or (empty-p to board)
             (eq (color to board) (opponent-color color))))))

(defun valid-queen-move-p (from to board color)
  (or (valid-rook-move-p   from to board color)
      (valid-bishop-move-p from to board color)))

;; TODO rewrite!!
(defun dangerous-p (pos color board)
  (loop named outer
     for row below 8 do
       (loop
          for col below 8
          with from = (cons col row) do
            (when (and (occupied-p from board)
                       (eq (color from board) (opponent-color color))
                       (valid-move-p (cons from pos) color board))
              (return-from outer t))))
  nil)

(defun valid-king-move-p (from to board color)
  (and (<= (abs (- (col to) (col from))) 1)
       (<= (abs (- (row to) (row from))) 1)
       (or (empty-p to board)
           (eq (color to board) (opponent-color color)))
       (not (dangerous-p to color board))))

(defun valid-move-p (move color board)
  (let ((from (from move))
        (to   (to move)))
    (and (not (equal from to))
         (not (empty-p from board))
         (eq (color from board) color)
         (case (kind from board)
           (B (valid-bishop-move-p from to board color))
           (K (valid-king-move-p   from to board color))
           (N (valid-knight-move-p from to board color))
           (P (valid-pawn-move-p   from to board color))
           (Q (valid-queen-move-p  from to board color))
           (R (valid-rook-move-p   from to board color))))))

(defun play-white (board)
  (loop
     for move = (read-move)
     until (valid-move-p move 'W board)
     finally (make-move move board)))

(defun copy-board (board)
  (let ((copy (make-array '(8 8))))
    (each-square row col
      (setf (aref copy row col)
            (aref board row col)))
    copy))

(defun rate-possible-moves (from board)
  (let ((score 0))
    (each-square row col
      (let* ((to   (cons col row))
             (move (cons from to)))
        (when (and (occupied-p to board)
                   (eq (color to board) 'W)
                   (valid-move-p move 'B board))
          (incf score (cdr (assoc (kind to board) *scores*))))))
    score))

(defun rate-board (board)
  (let ((score 0))
    (each-square row col
      (let ((pos (cons col row)))
        (when (and (occupied-p pos board)
                   (eq (color pos board) 'B))
          (incf score (rate-possible-moves pos board)))))
    score))

(defun minimax (depth max-depth color board)
  (if (= depth max-depth)
      (values (rate-board board) nil)
      (let ((best-score nil)
            (best-move  nil))
        (when *debug*
          (format t "~vtminimax ~a:~%" (* 3 depth) color))
        (each-square row col
          (let ((from (cons col row)))
            (when (and (occupied-p from board)
                       (eq (color from board) color))
              (each-square to-row to-col
                (let ((move (cons from (cons to-col to-row))))
                  (when (valid-move-p move color board)
                    (when *debug*
                      (format t "~vt  considering ~a~%" (* 3 depth) (format-move move)))
                    (let ((board-copy (copy-board board)))
                      (make-move move board-copy)
                      (let ((score (minimax (1+ depth) max-depth (opponent-color color) board-copy)))
                        (when *debug*
                          (format t "~vt  scores ~d~%" (* 3 depth) score))
                        (when (or (null best-score)
                                  (and (eq color 'B)
                                       (> score best-score))
                                  (and (eq color 'W)
                                       (< score best-score)))
                          (setf best-score score
                                best-move  move))))))))))
        (when *debug*
          (format t "~vt  winner ~a with score ~d~%" (* 3 depth) (format-move best-move) best-score))
        (values best-score best-move))))

(defun format-move (move)
  (let* ((from     (car move))
         (to       (cdr move))
         (from-col (car from))
         (from-row (cdr from))
         (to-col   (car to))
         (to-row   (cdr to)))
    (format nil "~(~a~d~a~d~)"
            (code-char (+ from-col 65))
            (1+ from-row)
            (code-char (+ to-col 65))
            (1+ to-row))))

(defun play-black (max-depth board)
  (multiple-value-bind (score move)
      (minimax 0 max-depth 'B board)
    (declare (ignore score))
    (format t "! ~a~%" (format-move move))
    (make-move move board)))

(defun play-game (&optional (max-depth 3))
  (let ((board (make-array '(8 8) :initial-contents *initial-board*)))
    (loop do
         (print-board board)
         (play-white board)
         (print-board board)
         (play-black max-depth board))))
