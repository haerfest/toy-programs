(use '[clojure.string :only (split split-lines)])

(def t
 (letfn [(parse-int [s] (Integer/parseInt s))]
   (vec (map #(vec (map parse-int (split % #"\s+")))
             (split-lines (slurp "triangle.txt"))))))

(def answer
  (memoize (fn [r c]
             (if (= r (count t))
               0
               (+ ((t r) c)
                  (max (answer (inc r) c)
                       (answer (inc r) (inc c))))))))
