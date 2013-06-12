(defn answer [upper-limit]

  (let [mem (atom {1 1})]

    (letfn [(collatz-len [n history]
              (if-let [len (get @mem n)]
                ; have memoized n already, update history
                (reduce (fn [len n] (let [len (inc len)]
                                      (swap! mem assoc n len)
                                      len))
                        len history)
                ; not memoized, recurse
                (recur (if (even? n)
                         (/ n 2)
                         (inc (* 3 n)))
                       (conj history n))))]

      (loop [n (dec upper-limit)
             winner 1
             winner-len 1]
        (if (zero? n)
          winner
          (let [len (collatz-len n '())]
            (if (> len winner-len)
              (recur (dec n) n len)
              (recur (dec n) winner winner-len))))))))
