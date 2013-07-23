(defn answer [upper-limit]

  (letfn [(collatz-len [n history mem]
            (if-let [len (get mem n)]
              ; have memoized n already, update history
              (loop [len     len
                     mem     mem
                     history history]
                (if (empty? history)
                  [len mem]
                  (recur (inc len)
                         (assoc! mem (first history) len)
                         (rest history))))
              ; not memoized, recurse
              (recur (if (even? n)
                       (/ n 2)
                       (inc (* 3 n)))
                     (conj history n)
                     mem)))]

    (loop [n          (dec upper-limit)
           winner     1
           winner-len 1
           mem        (transient {1 1})]
      (if (zero? n)
        winner
        (let [[len mem] (collatz-len n '() mem)]
          (if (> len winner-len)
            (recur (dec n) n      len        mem)
            (recur (dec n) winner winner-len mem)))))))
