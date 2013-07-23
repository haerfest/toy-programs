(use '[clojure.string :only (split)])

(def names (sort (map #(subs % 1 (dec (count %)))
                      (split (slurp "names.txt") #","))))

(defn upperchar->score [c]
  (inc (- (int c) (int \A))))

(defn name->score [name]
  (reduce + (map upperchar->score name)))

(defn score [[name index]]
  (* (name->score name) index))

(defn answer []
  (reduce + (map score (zipmap names (iterate inc 1)))))
