(dolist (package '("drakma" "plump" "clss" "do-urlencode" "parse-float" "bordeaux-threads"))
  (ql:quickload package))

(use-package :parse-float)

(defstruct result
  platform
  title
  score
  user-score)

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

(defun parse-details-link (node)
  "Returns a link to the details page."
  (concatenate 'string
               "https://www.metacritic.com"
               (plump:attribute (elt (clss:select "h3.basic_stat a" node) 0) "href")))

(defun get-user-score (url)
  "Returns a game's user score as a float or NIL." 
  (let* ((body  (drakma:http-request url :user-agent :explorer))
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
  (let* ((url     (concatenate 'string
                               "https://www.metacritic.com/search/game/"
                               (do-urlencode:urlencode title)
                               "/results?page="
                               (write-to-string (1- page))))
         (body    (drakma:http-request url :user-agent :explorer))
         (dom     (plump:parse body))
         (count   (parse-page-count dom))
         (nodes   (clss:select "li.result" dom))
         (results (loop for node across nodes
                     ;; temporarily store the details url as the user-score
                     collect (make-result :platform   (parse-platform node)
                                          :title      (parse-title node)
                                          :score      (parse-score node)
                                          :user-score (parse-details-link node)))))
    ;; spawn an army of threads to fetch user scores in parallel; since each
    ;; thread is working on its own entry in the list, no locking is necessary
    (let ((threads (loop for index below (length results)
                      collect (bordeaux-threads:make-thread
                               (lambda ()
                                 ;; replace the temporary url in user-score by the
                                 ;; actual user-score
                                 (let ((url (result-user-score (nth index results))))
                                   (setf (result-user-score (nth index results))
                                         (if user-scores
                                             (get-user-score url)
                                             nil))))))))
      ;; wait for all threads to finish
      (loop for thread in threads
         do (bordeaux-threads:join-thread thread)))
    (values results count)))
