(ns daily-verse.core
  (:require [clj-http.client :as client]))

(defn -main []
  (let [url "http://www.esvapi.org/v2/rest/dailyVerse?key=IP&output-format=plain-text&include-first-verse-numbers=false&include-verse-numbers=false&include-footnotes=false&include-short-copyright=false&include-passage-horizontal-lines=false&include-heading-horizontal-lines=false&include-headings=false&include-subheadings=false"]
    (println (:body (client/get url)))))
