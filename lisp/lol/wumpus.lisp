(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)
(defparameter *player-pos* nil)

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
  (draw-city))

(defun find-empty-node ()
  "Returns a node without any occupants."
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

(defun draw-city ()
  "Saves a PNG of the generated city graph."
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))
