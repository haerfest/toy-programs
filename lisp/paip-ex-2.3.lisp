(defparameter *dutch*
  '((zin -> (onderwerp persoonsvorm lijdend-voorwerp))
    (onderwerp -> (lidwoord zelfstandig-naamwoord))
    (persoonsvorm -> kust pakt slaat gebruikt ziet ruikt eet)
    (lijdend-voorwerp -> (lidwoord zelfstandig-naamwoord))
    (lidwoord -> de het een)
    (zelfstandig-naamwoord -> man vrouw bal iphone kast fiets)))

(defvar *grammar* *dutch*)

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
  (if (listp phrase)
    (mappend #'generate phrase)
    (let ((choices (rewrites phrase)))
      (if (null choices)
        (list phrase)
        (generate (random-elt choices))))))

