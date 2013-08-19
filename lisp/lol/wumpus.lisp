(load "graph-util")

(defparameter *congestion-city-nodes* nil
  "An alist describing the nodes in the city.  Each node's list can carry extra
  items that indicate what can be seen or heard from that node: 'BLOOD!
  indicates a blood trail from the Wumpus, indicating the Wumpus is at most two
  nodes away. 'GLOW-WORM indicates the presence of a glow-worm gang. 'LIGHTS!
  indicates the presence of a glow-worm gang one node away. 'SIRENS! indicates
  the presence of a cop on a direct edge.  E.g. '((1 BLOOD!) (2) (3 GLOW-WORM)
  (4) (5) (6 BLOOD! LIGHTS! SIRENS!) ... (27) (28 BLOOD!) (29 SIRENS!) (30)).")
  
(defparameter *congestion-city-edges* nil
  "An alist describing the edges in the city.  Each edge's list carries each
  node that is directly reachable as a sublist, and and reachable node's list
  can contain a second item 'COPS that indicate that that edge has police
  stationed there.  E.g. '((16 (1) (6 COPS)) (14 (15) (11)) (7 (20)) ...).")

(defparameter *visited-nodes* nil
  "A list of nodes visited by the player.  E.g. '(30 27).")

(defparameter *node-num* 30
  "The number of nodes in the city.")

(defparameter *edge-num* 45
  "The number of edges between nodes.")

(defparameter *worm-num* 3
  "The number of glow-worms in the city.")

(defparameter *cop-odds* 15
  "The 1:n odds that an edge has police stationed there.")

(defparameter *player-pos* nil
  "The node number that the player is currently at.  E.g. 30.")

(defun random-node ()
  "Returns a random node number between 1 and *node-num*."
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  "Returns the two edges between nodes a and b as '((a . b) (b . a))
   unless a and b are identical."
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  "Returns a list with *edge-num* edge pairs."
  (apply #'append (loop repeat *edge-num*
                     collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  "Returns all direct edges of node.  E.g. (direct-edges 10 (make-edge-list))
  might yield '((10 . 24) (10 . 1) (10 . 23))."
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  "Returns a list of all nodes that can be reached from node."
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  "Returns a list with islands, each of which is a list of nodes that are
  connected. Islands do not share connected nodes."
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  "Returns a list of edge pairs to connect all islands."
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  "Returns edge-list with extra edge pairs added to make each node reachable
  from each other node."
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  "Creates and returns an alist of city edges and places cops."
  (let* ((nodes (loop for i from 1 to *node-num*
                   collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (declare (ignore x))
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  "Returns an alist for a list of edges.  E.g. (edges-to-alist
  '((1 . 2) (2 . 1) (2 . 3) (3 . 2))) will produce
  '((1 (2)) (2 (1 3)) (3 (2))), indicating for instance that from
  node 2 one can get directly to nodes 1 and 3."
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  "Marks edges in the edge-alist that contains cops.  E.g. a node 1 that has
  edges to nodes 2 to 4, and the edge to 3 has a cop, will be represented as
  '((1 (2) (3 COPS) (4)))."
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

(defun neighbors (node edge-alist)
  "Returns the neighboring nodes for a node.  C.f. #'direct-edges."
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  "Returns whether nodes a and b are neighbors."
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  "Returns whether nodes a and b are neighbors or share a neighbor."
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  "Builds and returns the final map of the city, placing the Wumpus,
  the glow worms, and the cops.  The map is an alist from node to
  a possible list of descriptions."
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                       collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
                       (cond ((eql n wumpus) '(wumpus))
                             ((within-two n wumpus edge-alist) '(blood!)))
                       (cond ((member n glow-worms)
                              '(glow-worm))
                             ((some (lambda (worm)
                                      (within-one n worm edge-alist))
                                    glow-worms)
                              '(lights!)))
                       (when (some #'cdr (cdr (assoc n edge-alist)))
                         '(sirens!))))))

(defun new-game ()
  "Starts a new game."
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun find-empty-node ()
  "Returns a node without any occupants."
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

(defun draw-city ()
  "Saves a PNG of the generated city graph."
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes ()
  "Returns a list with the nodes currently known to the player.  These include
  the node the player is in, any visited nodes, and neighboring nodes to those.
  E.g. (known-city-nodes) may yield '((30 *) (27 ?) (26 ?) (29 ?))."
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))  ; node containing the player
                      n))
                ;; not visited yet
                (list node '?)))
          (remove-duplicates
           (append *visited-nodes*  ; player sees all visited nodes
                   ;; and all nodes neighboring the ones we visited
                   (mapcan (lambda (node)
                             (mapcar #'car
                                     (cdr (assoc node
                                                 *congestion-city-edges*))))
                           *visited-nodes*)))))

(defun known-city-edges ()
  "Return an alist with the known edges, stripping the cops from any edges we
  haven't visited yet."
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun walk (pos)
  "Walks the player to a certain node."
  (handle-direction pos nil))

(defun charge (pos)
  "Lets the player charge a certain node, presuming the Wumpus is there."
  (handle-direction pos t))

(defun handle-direction (pos charging)
  "Handles the player walking or charging (when t) to a certain node."
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
        (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  "Handles the player moving to a new node via an edge, either walking there or
  charging (when t)."
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                     (princ "You found the Wumpus!")
                                     (princ "You ran into the Wumpus.")))
          (charging (princ "You wasted your last bulle. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))
