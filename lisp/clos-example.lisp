;;;;    A simple CLOS example.

;;; The animal super class and two methods.

(defclass animal ()
  ((kind :initarg :kind :reader kind))
  (:documentation "Represents an animal of a particular :kind."))

(defmethod speak ((a animal))
  "Lets the animal speak."
  nil)

(defmethod move ((a animal))
  "Lets the animal move."
  nil)

;;; A cat.

(defclass cat (animal) ())

(defmethod initialize-instance :around ((c cat) &rest _)
  (call-next-method c :kind 'cat))

(defmethod speak ((c cat))
  'purr)

(defmethod move ((c cat))
  'sneak)

;;; A bird.

(defclass bird (animal) ())

(defmethod initialize-instance :around ((b bird) &rest _)
  (call-next-method b :kind 'bird))

(defmethod speak ((b bird))
  'chirp)

(defmethod move ((b bird))
  'fly)

;;; Example.

(setq sylvester (make-instance 'cat))
(setq tweety (make-instance 'bird))

(kind sylvester)   ; => CAT
(speak sylvester)  ; => PURR
(move sylvester)   ; => SNEAK

(kind tweety)      ; => BIRD
(speak tweety)     ; => CHIRP
(move tweety)      ; => FLY
