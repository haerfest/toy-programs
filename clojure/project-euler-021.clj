(defn d [n]
  (inc (reduce + (flatten (for [x (range 2 (int (Math/sqrt n))) :when (zero? (rem n x))] [x (quot n x)])))))

(defn answer []
  (reduce + (for [n (range 1 10000)
                  :when (let [m (d n)] (and (not= n m)
                                            (= n (d m))))]
              n)))
