;;; Simplest definition.

(defclass person ()
  (name age gender))

(defparameter *x* (make-instance 'person))

(slot-exists-p *x* 'name)               ; => T
(slot-value *x* 'name)                  ; => UNBOUND-SLOT
(setf (slot-value *x* 'name) "John Doe")
(slot-value *x* 'name)                  ; => "John Doe"

;;; Allow initial arguments.

(defclass person ()
  ((name   :initarg :name)
   (age    :initarg :age)
   (gender :initarg :gender)))

(defparameter *x*
  (make-instance 'person
                 :name   "John Doe"
                 :age    100
                 :gender 'male))

(slot-value *x* 'name)                  ; => "John Doe"

;;; Create accessors.

(defclass person ()
  ((name    :initarg :name    :accessor name)
   (age     :initarg :age     :accessor age)
   (gender  :initarg :gender  :accessor gender)))

(defparameter *x*
  (make-instance 'person
                 :name   "John Doe"
                 :age    100
                 :gender 'male))

(name *x*)                              ; => "John Doe"
(age *x*)                               ; => 100
(gender *x)                             ; => MALE

(incf (age *x*))                        ; => 101
(age *x*)                               ; => 101

;;; Set types for optimization, not necessarily for type-checking.

(defclass person ()
  ((name    :initarg :name    :accessor name    :type string)
   (age     :initarg :age     :accessor age     :type fixnum)
   (gender  :initarg :gender  :accessor gender  :type symbol)))

(defparameter *x*
  (make-instance 'person
                 :name   "John Doe"
                 :age    100
                 :gender 'male))

(type-of (name *x*))                    ; => SIMPLE-ARRAY-CHARACTER
(type-of (age *x*))                     ; => INTEGER
(type-of (gender *x*))                  ; => SYMBOL

(setf (age *x*) 'ONE-HUNDRED)           ; => ONE-HUNDRED
(type-of (age *x*))                     ; => SYMBOL

;;; Add type-checking at instantiation time.  The result of storing a value in
;;; a slot of a different type is undefined.

(locally (declare (optimize safety))
  (defclass person ()
    ((name    :initarg :name    :accessor name    :type string)
     (age     :initarg :age     :accessor age     :type fixnum)
     (gender  :initarg :gender  :accessor gender  :type symbol))))

(defparameter *x*
  (make-instance 'person
                 :name   "John Doe"
                 :age    'ONE-HUNDRED
                 :gender 'male))        ; => TYPE-ERROR

(defparameter *x*
  (make-instance 'person
                 :name   "John Doe"
                 :age    100
                 :gender 'male))

(setf (age *x*) 'ONE-HUNDRED)
(type-of (age *x*))                     ; => SYMBOL
(check-type (age *x*) fixnum)           ; => SIMPLE-TYPE-ERROR

(setf (age *x*) 100)
(check-type (age *x*) fixnum)           ; => NIL
