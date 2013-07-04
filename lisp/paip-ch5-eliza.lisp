(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do.  Please state your problem.))

    (((?* ?x) hi (?* ?y))
     (How are you.  How may I help you?))

    (((?* ?x) weather (?* ?y))
     (What about the weather?)
     (I am afraid I have not been outside today)
     (Do you like the weather today?))

    (((?* ?x) I want (?* ?y))
     (What would it mean to you if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))

    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y)
     (Do you wish that ?y)
     (What do you think about ?y)
     (Really -- if ?y))

    (((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit negative)
     (Are you saying "NO" just to be negative?))

    (((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))

    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))

    (((? ?x) I felt (? ?y))
     (What other feelings do you have?))))

(defconstant fail nil
             "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
             "Indicates pat-match success, with no variables.")

(defun get-binding (var bindings)
  "Find a (var . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings.
        (if (eq bindings no-bindings)
          nil
          bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
      (match-variable var input bindings)
      ;; We assume that pat starts with a constant
      ;; In other words, a pattern can't have 2 consecutive vars
      (let ((pos (position (first pat) input
                           :start start :test #'equal)))
        (if (null pos)
          fail
          (let ((b2 (pat-match
                      pat (subseq input pos)
                      (match-variable var (subseq input 0 pos)
                                      bindings))))
            ;; If this match failed, try another longer one
            (if (eq b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              b2)))))))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)."
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings."
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

(defun rule-pattern (rule) (first rule))

(defun rule-responses (rule) (rest rule))

(defun skip-invalid (input start)
  "Returns the first position in input starting at start with a valid character."
  (if (>= start (length input))
    start
    (case (char input start)
      ((#\, #\" #\;) (skip-invalid-input input (1+ start)))
      (otherwise start))))

(defun read-input (input &optional (start 0) (the-list nil))
  "Reads input from the user and returns it as a list."
  (if (>= start (length input))
    the-list
    (multiple-value-bind (element pos) (read-from-string input t nil :start start)
      (read-input input (skip-invalid input pos) (append the-list (list element))))))

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)
    (write (flatten (use-eliza-rules (read-input (read-line)))) :pretty t)))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                (sublis (switch-viewpoint result)
                        (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))

(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
    x
    (list x)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))
