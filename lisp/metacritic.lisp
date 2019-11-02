;; (ql:quickload :drakma)
;; (ql:quickload :plump)
;; (ql:quickload :clss)
;; (ql:quickload :do-urlencode)

(defun parse-platform (node)
  "Returns a game's platform."
  (plump:text (elt (clss:select "span.platform" node) 0)))

(defun parse-title (node)
  "Returns a game's title."
  (string-trim '(#\Space #\t #\newline)
               (plump:text (elt (clss:select "h3.product_title a" node)
                                0))))

(defun parse-score (node)
  "Returns a game's review score as an integer or NIL." 
  (let ((score (plump:text (elt (clss:select "span.metascore_w" node) 0))))
    (parse-integer score :junk-allowed t)))

(defun parse-page-count (dom)
  "Returns the total number of search result pages."
  (loop
     for tag in '(a span)
     for selector = (concatenate 'string
                                 "li.last_page "
                                 (write-to-string tag :case :downcase)
                                 ".page_num")
     for nodes = (clss:select selector dom)
     when (> (length nodes) 0)
     return (parse-integer (plump:text (elt nodes 0)))
     finally (return 1)))

(defun get-scores (title &key (page 1))
  "Fetches game review scores for game TITLE from the Metacritic webpage for a
   single PAGE number. Returns multiple values: the search results as a list of
   (PLATFORM TITLE SCORE), with score being NIL when not available, and the
   total number of search result pages."
  (let* ((url (concatenate 'string
                           "https://www.metacritic.com/search/game/"
                           (do-urlencode:urlencode title)
                           "/results?page="
                           (write-to-string (1- page))))
         (body (drakma:http-request url :user-agent :explorer))
         (dom (plump:parse body))
         (nodes (clss:select "li.result" dom))
         (results (loop for node across nodes
                     collect (list (parse-platform node)
                                   (parse-title node)
                                   (parse-score node))))
         (count (parse-page-count dom)))
    (values results count)))
