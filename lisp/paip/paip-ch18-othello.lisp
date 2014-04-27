(defconstant all-directions '(-11 -10 -9 -1 1 9 10 11))

(defconstant empty 0 "An empty square")
(defconstant black 1 "A black piece")
(defconstant white 2 "A white piece")
(defconstant outer 3 "Marks squares outside the 8x8 board")

(deftype piece () `(integer ,empty ,outer))

(defun name-of (piece) (char ".@O?" piece))

(defun opponent (player) (if (eql player black) white black))

(deftype board () `(simple-array piece (100)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

(defun copy-board (board)
  (copy-seq board))

(defconstant all-squares
  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))

(defun initial-board ()
  "Return a board, empty except for four pieces in the middle."
  ;; Boards are 100-element vectors, with elements 11-88 used,
  ;; and the others marked with the sentinel OUTER.  Initially
  ;; the 4 center squares are taken, the others empty.
  (let ((board (make-array 100 :element-type 'piece
                           :initial-element outer)))
    (dolist (square all-squares)
      (setf (bref board square) empty))
    (setf (bref board 44) white   (bref board 45) black
          (bref board 54) black   (bref board 55) white)
    board))

(defun print-board (board)
  "Print a board, along with some statistics."
  (format t "~2&     1 2 3 4 5 6 7 8  [~c=~2a ~c=~2a (~@d)]"
          (name-of black) (count black board)
          (name-of white) (count white board)
          (count-difference black board))
  (loop for row from 1 to 8 do
       (format t "~&  ~d " (* 10 row))
       (loop for col from 1 to 8
          for piece = (bref board (+ col (* 10 row)))
          do (format t "~c " (name-of piece))))
  (format t "~2&"))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count player board)
     (count (opponent player) board)))

(defun valid-p (move)
  "Valid moves are numbers in the range 11-88 that end in 1-8."
  (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defun legal-p (move player board)
  "A Legal move must be into an empty square, and it must
  flip at least one opponent piece."
  (and (eql (bref board move) empty)
       (some (lambda (dir) (would-flip? move player board dir))
             all-directions)))

(defun make-move (move player board)
  "Update board to reflect move by player."
  ;; First make the move, then make any flips.
  (setf (bref board move) player)
  (dolist (dir all-directions)
    (make-flips move player board dir))
  board)

(defun make-flips (move player board dir)
  "Make any flips in the given direction."
  (let ((bracketer (would-flip? move player board dir)))
    (when bracketer
      (loop for c = (+ move dir) then (+ c dir) until (eql c bracketer)
         do (setf (bref board c) player)))))

(defun would-flip? (move player board dir)
  "Would this move result in any flips in this direction?
  If so, return the square number of the bracketing piece."
  ;; A flip occurs if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, bracketed by
  ;; one of player's pieces.
  (let ((c (+ move dir)))
    (and (eql (bref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(defun find-bracketing-piece (square player board dir)
  "Return the square number of the bracketing piece."
  (cond ((eql (bref board square) player) square)
        ((eql (bref board square) (opponent player))
         (find-bracketing-piece (+ square dir) player board dir))
        (t nil)))

(defun othello (bl-strategy wh-strategy &optional (print t))
  "Play a game of Othello.  Return the score, where a positive
  difference means black (the first player) wins."
  (let ((board (initial-board)))
    (loop for player = black
       then (next-to-play board player print)
       for strategy = (if (eql player black)
                          bl-strategy
                          wh-strategy)
       until (null player)
       do (get-move strategy player board print))
    (when print
      (format t "~&The game is over.  Final result:")
      (print-board board))
    (count-difference black board)))

(defun next-to-play (board previous-player print)
  "Compute the player to move next, or NIL if nobody can move."
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? opp board) opp)
          ((any-legal-move? previous-player board)
           (when print
             (format t "~&~c has no moves and must pass."
                     (name-of opp)))
           previous-player)
          (t nil))))

(defun any-legal-move? (player board)
  "Does player have any legal moves in this position?"
  (some (lambda (move) (legal-p move player board))
        all-squares))

(defun get-move (strategy player board print)
  "Call the player's strategy function to get a move.
  Keep calling until a legal move is made."
  (when print (print-board board))
  (let ((move (funcall strategy player (copy-board board))))
    (cond
      ((and (valid-p move) (legal-p move player board))
       (when print
         (format t "~&~c moves to ~d." (name-of player) move))
       (make-move move player board))
      (t (warn "Illegal move: ~d" move)
         (get-move strategy player board print)))))

(defun human (player board)
  "A human player for the game of Othello."
  (declare (ignore board))
  (format t "~&~c to move: " (name-of player))
  (read))

(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))

(defun legal-moves (player board)
  "Returns a list of legal moves for player."
  (loop for move in all-squares
     when (legal-p move player board) collect move))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun maximize-difference (player board)
  "A strategy that maximizes the difference in pieces."
  (funcall (maximizer #'count-difference) player board))

(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move,
  apply EVAL-FN to each resulting board, and choose
  the move for which EVAL-FN returns the best score.
  FN takes two arguments: the player-to-move and board."
  (lambda (player board)
    (let* ((moves (legal-moves player board))
           (scores (mapcar #'(lambda (move)
                               (funcall
                                eval-fn
                                player
                                (make-move move player
                                           (copy-board board))))
                           moves))
           (best (apply #'max scores)))
      (elt moves (position best scores)))))

(defun ex-18-1 ()
  "Exercise 18.1: Play some games with maximize-difference against
  random-strategy and humn.  How good is maximize-difference?"
  (evaluate-strategies #'maximize-difference #'random-strategy))

(defun evaluate-strategies (a b)
  (loop repeat 100
     for score = (othello a b nil)
     sum score))

(defparameter *weights*
  '#(0   0   0  0  0  0  0   0   0 0
     0 120 -20 20  5  5 20 -20 120 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0  20  -5 15  3  3 15  -5  20 0
     0   5  -5  3  3  3  3  -5   5 0
     0   5  -5  3  3  3  3  -5   5 0
     0  20  -5 15  3  3 15  -5  20 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0 120 -20 20  5  5 20 -20 120 0
     0   0   0  0  0  0  0   0   0 0))

(defun weighted-squares (player board)
  "Sum of the weights of a player's squares minus opponent's."
  (let ((opp (opponent player)))
    (loop for i in all-squares
       when (eql (bref board i) player)
       sum (aref *weights* i)
       when (eql (bref board i) opp)
       sum (- (aref *weights* i)))))

(defun ex-18-2 ()
  (values (evaluate-strategies (maximizer #'weighted-squares)
                               (maximizer #'count-difference))
          (evaluate-strategies (maximizer #'count-difference)
                               (maximizer #'weighted-squares))))

(defconstant winning-value most-positive-fixnum)
(defconstant losing-value  most-negative-fixnum)

(defun final-value (player board)
  "Is this a win, loss, or draw for player?"
  (case (signum (count-difference player board))
    (-1 losing-value)
    ( 0 0)
    (+1 winning-value)))

(defun minimax (player board ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (minimax (opponent player) board
                            (- ply 1) eval-fn))
                (final-value player board))
            (let ((best-move nil)
                  (best-val nil))
              (dolist (move moves)
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (minimax (opponent player) board2
                                        (- ply 1) eval-fn))))
                  (when (or (null best-val)
                            (> val best-val))
                    (setf best-val val)
                    (setf best-move move))))
              (values best-val best-move))))))

(defun minimax-searcher (ply eval-fn)
  "A strategy that searches PLY levels and then uses EVAL-FN."
  (lambda (player board)
    (multiple-value-bind (value move)
        (minimax player board ply eval-fn)
      (declare (ignore value))
      move)))

(defun alpha-beta (player board achievable cutoff ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values,
  using cutoffs whenever possible."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (alpha-beta (opponent player) board
                               (- cutoff) (- achievable)
                               (- ply 1) eval-fn))
                (final-value player board))
            (let ((best-move (first moves)))
              (loop for move in moves
                 do (let* ((board2 (make-move move player
                                              (copy-board board)))
                           (val (- (alpha-beta (opponent player) board2
                                               (- cutoff) (- achievable)
                                               (- ply 1) eval-fn))))
                      (when (> val achievable)
                        (setf achievable val)
                        (setf best-move move)))
                 until (>= achievable cutoff))
              (values achievable best-move))))))

(defun alpha-beta-searcher (depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  (lambda (player board)
    (multiple-value-bind (value move)
        (alpha-beta player board losing-value winning-value
                    depth eval-fn)
      (declare (ignore value))
      move)))
