(dolist (package '("drakma" "plump" "clss" "do-urlencode" "parse-float"))
  (ql:quickload package))

(use-package :parse-float)

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

(defun parse-user-score (node stream)
  "Returns a game's user score as a float or NIL." 
  (let* ((links (clss:select "h3.basic_stat a" node))
         (url   (concatenate 'string
                             "https://www.metacritic.com"
                             (plump:attribute (elt links 0) "href")))
         (body  (drakma:http-request url :user-agent :explorer :stream stream :close nil))
         (dom   (plump:parse body))
         (nodes (clss:select "div.user" dom)))
    (if (zerop (length nodes))
        nil
        (let ((score (plump:text (elt nodes 0))))
          (parse-float score :junk-allowed t)))))

(defun parse-page-count (dom)
  "Returns the total number of search result pages."
  (loop
     for tag in '(a span)
     for selector = (concatenate 'string
                                 "li.last_page "
                                 (write-to-string tag :case :downcase)
                                 ".page_num")
     for nodes = (clss:select selector dom)
     when (not (zerop (length nodes)))
     return (parse-integer (plump:text (elt nodes 0)))
     finally (return 1)))

(defun get-scores (title &key (page 1) (user-scores nil))
  "Fetches game review scores for game TITLE from the Metacritic webpage for a
   single PAGE number. When USER-SCORES is non-NIL, also fetches user scores.
   Returns multiple values: the search results as a list of (PLATFORM TITLE
   SCORE USER-SCORE), and the total number of search result pages. SCORE and
   USER-SCORE can be NIL when not available or not fetched."
  (let ((url (concatenate 'string
                          "https://www.metacritic.com/search/game/"
                          (do-urlencode:urlencode title)
                          "/results?page="
                          (write-to-string (1- page)))))
    (multiple-value-bind (body status-code headers uri stream) (drakma:http-request url :user-agent :explorer :close nil)
      (declare (ignore status-code headers uri))
      (let* ((dom     (plump:parse body))
             (count   (parse-page-count dom))
             (nodes   (clss:select "li.result" dom))
             (results (loop for node across nodes
                         collect (list (parse-platform node)
                                       (parse-title node)
                                       (parse-score node)
                                       (if (null user-scores)
                                           nil
                                           (parse-user-score node stream))))))
        (close stream)
        (values results count)))))
