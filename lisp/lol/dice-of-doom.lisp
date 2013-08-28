(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(setf *random-state* (make-random-state t))

(defun board-array (lst)
  "Returns the game board lst as an array."
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  "Generates a random game board."
  (board-array (loop for n below *board-hexnum*
                  collect (list (random *num-players*)
                                (1+ (random *max-dice*))))))

(defun player-letter (n)
  "Returns a character representing a player, #\a for player 0, etc."
  (code-char (+ 97 n)))

(defun draw-board (board)
  "Prints the hexagonal game board, each field of the form a-3, indicating that
  player 0 (#\a) has three dice stacked on that field.  For example:
    b-1 b-2 
  a-2 b-2"
  (loop for y below *board-size*
     do (progn (fresh-line)
               (loop repeat (- *board-size* y) ; indent top rows
                  do (princ "  "))
               (loop for x below *board-size*
                  for hex = (aref board (+ x (* *board-size* y)))
                  do (format t "~a-~a "
                             (player-letter (first hex))
                             (second hex))))))

(defun game-tree-naive (board player spare-dice first-move)
  "Generates the entire game tree as a nested list, each level representing a
  possible game state: '(player board moves).  Moves is a list of
  '(description game-tree)."
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves)
  "Returns the possible moves in case the player passes.  Moves is the list of
  possible attack moves."
  (if first-move
      moves                             ; cannot pass on first move
      (cons (list nil                   ; no description
                  (game-tree (add-new-dice board player
                                           (1- spare-dice)) ; reinforcements
                             (mod (1+ player) *num-players*) ; next player
                             0
                             t))
            moves)))

(defun attacking-moves (board cur-player spare-dice)
  "Returns the possible attack moves for a player."
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                ;; player occupies the src hexagon
                (mapcan (lambda (dst)
                          ;; attack opponent's hex when player has more dice
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                             (list (list src dst)
                                   (game-tree (board-attack board
                                                            cur-player
                                                            src dst
                                                            (dice src))
                                              cur-player
                                              (+ spare-dice (dice dst))
                                              nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
               collect n))))

(defun neighbors-naive (pos)
  "Returns the neighboring hexagons on the game board for a given pos.  A
  hexagon can have up to six neighbors."
  (let ((up   (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
       when (and (>= p 0) (< p *board-hexnum*))
         collect p)))

(defun board-attack (board player src dst dice)
  "Returns the game board in case the player in hexagon src attacks the player
  in hexagon dst."
  (board-array (loop for pos from 0
                  for hex across board
                  collect (cond ((eq pos src) (list player 1))
                                ((eq pos dst) (list player (1- dice)))
                                (t hex)))))

(defun add-new-dice (board player spare-dice)
  "Returns the game board when spare-dice are added for the player."
  (labels ((f (lst n acc)
             (cond ((zerop n) (append (reverse acc) lst))
                   ((null lst) (reverse acc))
                   (t (let ((cur-player (caar lst))
                            (cur-dice   (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (f (cdr lst)
                               (1- n)
                               (cons (list cur-player (1+ cur-dice)) acc))
                            (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))

(defun play-vs-human (tree)
  "Plays the game against another human player.  Example usage:
  (play-vs-human (game-tree (gen-board) 0 0 t))."
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun print-info (tree)
  "Prints the player whose turn it is, and the game board."
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun handle-human (tree)
  "Let's a human player select a possible move and returns that move."
  (fresh-line)
  (princ "choose your move: ")
  (let ((moves (caddr tree)))
    (loop for move in moves
       for n from 1
       do (let ((action (car move)))
            (fresh-line)
            (format t "~a. " n)
            (if action
                (format t "~a -> ~a" (car action) (cadr action))
                (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

(defun winners (board)
  "Returns a list of winners."
  (let* ((tally (loop for hex across board
                   collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  "Prints the winners."
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))

(defun rate-position-naive (tree player)
  "Returns a rating for a player's game position, using the minimax algorithm."
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        ;; no more moves here
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))

(defun get-ratings (tree player)
  "Returns a list with rates for all moves a player can make."
  (mapcar (lambda (move)
            (rate-position (cadr move) player))
          (caddr tree)))

(defun handle-computer (tree)
  "Let's a computer player select a possible move and returns that move."
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defun play-vs-computer (tree)
  "Plays the game against a computer player.  Example usage:
  (play-vs-computer (game-tree (gen-board) 0 0 t))."
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

;;; Memoization

(let ((previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (neighbors-naive pos)))))

(let ((previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply #'game-tree-naive rest)))))

(let ((previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (rate-position-naive tree player))))))
