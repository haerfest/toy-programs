;; To install and run MongoDB:
;; $ brew install mongodb
;; $ mongod --config /usr/local/etc/mongod.conf
;;
;; To install the Lisp packages:
;; > (ql:quickload '(:cl-who :hunchentoot :parenscript :cl-mongo))

(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo))

(in-package :retro-games)

(defparameter *game-collection* "game")

(cl-mongo:db.use "games")

(defun unique-index-on (field)
  (db.ensure-index *game-collection*
                   ($ field 1)
                   :unique t))

(unique-index-on "name")

(defclass game ()
  ((name :reader name
         :initarg :name)
   (votes :accessor votes
          :initarg :votes
          :initform 0)))

(defun doc->game (game-doc)
  (make-instance 'game
                 :name (get-element "name" game-doc)
                 :votes (get-element "votes" game-doc)))

(defun game->doc (game)
  ($ ($ "name" (name game))
     ($ "votes" (votes game))))

(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))

(defmethod vote-for :after (game)
  (let ((game-doc (game->doc game)))
    (db.update *game-collection* ($ "name" (name game)) game-doc)))

(defun game-from-name (name)
  (let ((found-games (docs (db.find *game-collection*
                                    ($ "name" name)))))
    (when found-games
      (doc->game (first found-games)))))

(defun game-stored? (name)
  (game-from-name name))

(defun games ()
  (mapcar #'doc->game
          (docs (iter
                 (db.sort *game-collection*
                          :all
                          :field "votes"
                          :asc nil)))))

(defun add-game (name)
  (let ((game (make-instance 'game :name name)))
    (db.insert *game-collection* (game->doc game))))

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))

(defmacro standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/retro.css")
             ,(when script
                    `(:script :type "text/javascript"
                              (str ,script))))
            (:body
             (:div :id "header"         ; retro games header
                   (:img :src "/logo.png"
                         :alt "Commodore 64"
                         :class "logo")
                   (:span :class "strapline"
                          "Vote on your favourite Retro Game"))
             ,@body))))

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page (:title "Retro Games")
    (:h1 "Vote on your all time favourite retro games!")
    (:p "Missing a game? Make it available for votes "
        (:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart"
          (:ol
           (dolist (game (games))
             (htm
              (:li (:a :href (format nil "vote?name=~a"
                                     (url-encode (name game)))
                       "Vote!")
                   (fmt "~A with ~d votes"
                        (escape-string (name game))
                        (votes game)))))))))

(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored? name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games"))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
                  :script (ps
                            (defvar add-form nil)
                            (defun validate-game-name (evt)
                              (when (= (@ add-form name value) "")
                                (chain evt (prevent-default))
                                (alert "Please enter a name.")))
                            (defun init ()
                              (setf add-form
                                    (chain document
                                           (get-element-by-id "addform")))
                              (chain add-form
                                     (add-event-listener "submit"
                                                         validate-game-name
                                                         false)))
                            (setf (chain window onload) init)))
    (:h1 "Add a new game to the chart")
    (:form :action "/game-added" :method "post" :id "addform"
           (:p "What is the name of the game?" (:br)
               (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))

