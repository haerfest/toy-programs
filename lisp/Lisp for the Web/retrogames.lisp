(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :retro-games)

(defclass game ()
  ((name :reader name
         :initarg :name)
   (votes :accessor votes
          :initform 0)))

(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))

(defvar *games* '())

(defun game-from-name (name)
  (find name *games* :test #'string-equal :key #'name))

(defun game-stored? (name)
  (game-from-name name))

(defun games ()
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name)
  (unless (game-stored? name)
    (push (make-instance 'game :name name) *games*)))

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))
