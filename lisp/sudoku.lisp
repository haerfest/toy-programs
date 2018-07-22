(defparameter *sudoku* '(- 9 - - - 4 - - -
                         - - - - 8 3 - - -
                         - - 2 6 - - 3 - -
                         - 5 4 - 7 - - 6 2
                         - - - 8 - - - - -
                         - - - - 2 - - - 9
                         1 - - - 6 - - 3 -
                         2 - 3 - 5 - 9 - -
                         8 - 5 - - 2 6 1 -))

;; Example usage:
;;
;; > (pprint (solve (as-matrix *sudoku*)))
;; #2A((3 9 6 7 1 4 5 2 8)
;;     (5 1 7 2 8 3 4 9 6)
;;     (4 8 2 6 9 5 3 7 1)
;;     (9 5 4 3 7 1 8 6 2)
;;     (6 2 1 8 4 9 7 5 3)
;;     (7 3 8 5 2 6 1 4 9)
;;     (1 7 9 4 6 8 2 3 5)
;;     (2 6 3 1 5 7 9 8 4)
;;     (8 4 5 9 3 2 6 1 7))


(defparameter *rows* (sqrt (length *sudoku*)) "Number of rows.")
(defparameter *cols* (sqrt (length *sudoku*)) "Number of columns.")

(defun as-matrix (sudoku)
  "Returns the SUDOKU puzzle as a two-dimensional matrix. Each cell is either
   an atom, representing a fixed digit, or a list of possible digits."
  (let ((a (make-array (list *rows* *cols*))))
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (setf (aref a row col)
              (let ((char (nth (+ (* row *rows*) col) sudoku)))
                (if (eq '- char)
                    (list 1 2 3 4 5 6 7 8 9) ; all digits are possible
                    char)))))                ; cell is fixed on this digit
    a))

(defun copy-sudoku (a)
  "Returns a copy of sudoku matrix A."
  (let ((b (make-array (list *rows* *cols*))))
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (setf (aref b row col) (aref a row col))))
    b))

(defun visit-while (sudoku row col rows cols f)
  "Applies the function F to the ROWS x COLS cells in SUDOKU starting at
  (ROW, COL), until either F returns NIL or the cells have all been visited.
  Returns whether all cells were visited."
  (dotimes (r rows)
    (dotimes (c cols)
      (let ((cell (aref sudoku (+ row r) (+ col c))))
        (when (not (funcall f cell))
          (return-from visit-while nil)))))
  t)

(defun remove-choice (digit cell)
  "Removes a DIGIT from the list of possibilities at CELL. If CELL is already
  fixed to a digit, it cannot be our DIGIT. Returns T upon success."
  (if (atom cell)
      (not (= digit cell))
      (setf cell (remove digit cell))))

(defun remove-choice-from-row (digit row sudoku)
  "Removes DIGIT as a choice from the entire ROW in SUDOKU. Returns T upon
  success."
  (visit-while sudoku row 0 1 *cols* (lambda (cell)
                                       (remove-choice digit cell))))

(defun remove-choice-from-col (digit col sudoku)
  "Removes DIGIT as a choice from the entire COLumn in SUDOKU. Returns T
  upon success."
  (visit-while sudoku 0 col *rows* 1 (lambda (cell)
                                       (remove-choice digit cell))))

(defun remove-choice-from-block (digit row col sudoku)
  "Removes DIGIT as a choice from the 3x3 block in SUDOKU which contains
  (ROW, COL). Returns T upon success."
  (let ((block-row (* 3 (truncate (/ row 3))))
        (block-col (* 3 (truncate (/ col 3)))))
    (visit-while sudoku block-row block-col 3 3
                 (lambda (cell)
                   (remove-choice digit cell)))))

(defun solve (sudoku &optional (row 0) (col 0))
  "Recursively solves the SUDOKU starting from (ROW, COL). Returns a solution
  or NIL if no solution could be found."
  (cond ((= *rows* row)                 ; end of the board, success
         sudoku)
        ((= *cols* col)                 ; end of column, move to next row
         (solve sudoku (+ row 1) 0))
        ((atom (aref sudoku row col))   ; a fixed digit, keep going
         (solve sudoku row (+ col 1)))
        ;; a cell with possibilities, try them one at a time until one leads to
        ;; a solution
        (t (dolist (digit (aref sudoku row col))
             (let ((sudoku* (copy-sudoku sudoku)))
               ;; verify that placing the digit does not give a conflict in the
               ;; same row, column, or block
               (when (and (remove-choice-from-row digit row sudoku*)
                          (remove-choice-from-col digit col sudoku*)
                          (remove-choice-from-block digit row col sudoku*))
                 ;; we're good to go, place the digit and solve from there
                 (setf (aref sudoku* row col) digit)
                 (let ((solution (solve sudoku* row (+ col 1))))
                   (when solution
                     (return-from solve solution)))))))))
