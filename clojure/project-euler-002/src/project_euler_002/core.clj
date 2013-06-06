(ns project-euler-002.core)

(defn sieve [upper-limit]
  (let [numbers (transient (vec (repeat upper-limit 'unmarked)))]

    (letfn [(sqr [x]
              (* x x))
            
            (mark-multiples-of [n]
              (loop [number (sqr n)]
                (when (< number upper-limit)
                  (assoc! numbers number 'marked)  ; number is not prime
                  (recur (+ number n)))))

            (find-next-number-to-mark [number]
              (loop [candidate number]
                (cond (>= (sqr candidate) upper-limit) 0  ; done with numbers
                      (= (numbers candidate) 'unmarked) candidate
                      :else (recur (inc candidate)))))

            (sum-primes []
              (loop [number 2
                     sum    0]
                (if (>= number upper-limit)
                  sum
                  (recur (inc number)
                         (if (= (numbers number) 'unmarked)
                           (+ sum number)
                           sum)))))]

      (loop [number 2]
        (if (zero? number)
          (sum-primes)
          (do
            (mark-multiples-of number)
            (recur (find-next-number-to-mark (inc number)))))))))
