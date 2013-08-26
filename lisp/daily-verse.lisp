(ql:quickload "trivial-http")

(defparameter *url*
  (concatenate 'string
               "http://www.esvapi.org/v2/rest/dailyVerse"
               "?key=IP"
               "&output-format=plain-text"
               "&include-short-copyright=false"
               "&include-verse-numbers=false"
               "&include-passage-horizontal-lines=false"))

(defun daily-verse ()
  "Retrieve the daily ESV bible verse."
  (let* ((result (trivial-http:http-get *url*))
         (code (first result)))
    (when (eq code 200)
      (let* ((headers (second result))
             (stream (third result))
             (length (parse-integer (rest (assoc :content-length headers))))
             (verse (make-array length :element-type 'character)))
        (read-sequence verse stream)
        (close stream)
        verse))))

(defun demo ()
  "Demonstrate usage."
  (format t (daily-verse)))
