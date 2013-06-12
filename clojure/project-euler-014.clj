(defn answer [upper-limit]

  (let [memory (atom (transient {1 1}))]

    (letfn [(collatz [n history mem]
              (if-let [len (get mem n)]
                ; have memoized n already, update history
                (loop [len len
                       mem mem
                       history history]
                  (if (empty? history)
                    [len mem]
                    (recur (inc len) (assoc! mem (first history) len) (rest history))))
                ; not memoized, recurse
                (recur (if (even? n)
                         (/ n 2)
                         (inc (* 3 n)))
                       (conj history n)
                       mem)))

            (collatz-len [n]
              (let [[len mem] (collatz n '() @memory)]
                (swap! memory (fn [_] mem))
                len))]

      (loop [n (dec upper-limit)
             winner 1
             winner-len 1]
        (if (zero? n)
          winner
          (let [len (collatz-len n)]
            (if (> len winner-len)
              (recur (dec n) n len)
              (recur (dec n) winner winner-len))))))))
