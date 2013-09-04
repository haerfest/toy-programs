(defparameter *simple-grammar*
  '((sentence    -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article     -> the a)
    (Noun        -> man ball woman table)
    (Verb        -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
  '((sentence    -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP*         -> () (PP PP*))
    (Adj*        -> () (Adj Adj*))
    (PP          -> (Prep noun-phrase))
    (Prep        -> to in by with on)
    (Adj         -> big little blue green adiabatic)
    (Article     -> the a)
    (Name        -> Pat Kim Lee Terry Robin)
    (Noun        -> man ball woman table)
    (Verb        -> hit took saw liked)
    (Pronoun     -> he she it these those that)))

(defvar *grammar* *bigger-grammar*
  "The grammar used by generate.  Initially, this is
   *simple-grammar*, but we can switch to other grammars.")

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun terminalp (rule)
  "Return whether a rule is a terminal symbol."
  (null (rewrites rule)))

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((terminalp phrase)
         (list phrase))
        (t (generate (random-elt (rewrites phrase))))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
   with a complete parse tree."
   (cond ((listp phrase)
          (mapcar #'generate-tree phrase))
         ((rewrites phrase)
          (cons phrase
                (generate-tree (random-elt (rewrites phrase)))))
         (t (list phrase))))
