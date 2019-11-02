;; (ql:quickload :drakma)

;; https://www.metacritic.com/search/game/majora%27s%20mask/results?plats[16]=1&search_type=advanced
(defun get-scores (title)
    (multiple-value-bind (body status headers uri stream must-close reason)
        (drakma:http-request "https://www.metacritic.com/search/game/majora%27s%20mask/results"
                             :user-agent :explorer)
      (with-open-file (stream "out.html"
                              :direction :output
                              :if-exists :supersede)
        (format stream body))))
