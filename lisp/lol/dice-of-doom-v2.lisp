(load "dice-of-doom-v1")
(load "lazy")

(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun add-passing-move (board player spare-dice first-move moves)
  "Returns the possible moves in case the player passes.  Moves is the list of
  possible attack moves."
  (if first-move
      moves                             ; cannot pass on first move
      (lazy-cons (list nil              ; no description
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
    (lazy-mapcan (lambda (src)
                   (if (eq (player src) cur-player)
                       ;; player occupies the src hexagon
                       (lazy-mapcan
                        (lambda (dst)
                          ;; attack opponent's hex when player has more dice
                          (if (and (not (eq (player dst) cur-player))
                                   (> (dice src) (dice dst)))
                              (make-lazy
                               (list (list (list src dst)
                                           (game-tree (board-attack board
                                                                    cur-player
                                                                    src dst
                                                                    (dice src))
                                                      cur-player
                                                      (+ spare-dice (dice dst))
                                                      nil))))
                              (lazy-nil)))
                        (make-lazy (neighbors src)))
                       (lazy-nil)))
                 (make-lazy (loop for n below *board-hexnum*
                               collect n)))))

(defun handle-human (tree)
  "Let's a human player select a possible move and returns that move."
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                       (format t "~a -> ~a" (car action) (cadr action))
                       (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  "Plays the game against another human player.  Example usage:
  (play-vs-human (game-tree (gen-board) 0 0 t))."
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

