;; (ql:quickload :drakma)
;; (ql:quickload :plump)
;; (ql:quickload :clss)

;; https://www.metacritic.com/search/game/majora%27s%20mask/results?plats[16]=1&search_type=advanced
(defun get-scores (title)
    (multiple-value-bind (body status headers uri stream must-close reason)
        (drakma:http-request "https://www.metacritic.com/search/game/majora%27s%20mask/results"
                             :user-agent :explorer)
      (let ((dom (plump:parse body)))
        (loop for result across (clss:select "li.result" dom)
           collect (list
                    (plump:text (elt (clss:select "span.platform" result) 0))
                    (string-trim '(#\Space #\t #\newline)
                                 (plump:text (elt (clss:select "h3.product_title a" result) 0)))
                    (plump:text (elt (clss:select "span.metascore_w" result) 0)))))))
