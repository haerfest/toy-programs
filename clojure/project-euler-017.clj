(defn say [n]
  (let [word { 1 'one      2 'two        3 'three     4 'four      5 'five
               6 'six      7 'seven      8 'eight     9 'nine     10 'ten
              11 'eleven  12 'twelve    13 'thirteen 14 'fourteen 15 'fifteen
              16 'sixteen 17 'seventeen 18 'eighteen 19 'nineteen 20 'twenty
              30 'thirty  40 'forty     50 'fifty    60 'sixty    70 'seventy
              80 'eighty  90 'ninety}
        thousands (quot n 1000)
        hundreds  (quot (- n (* thousands 1000)) 100)
        tens      (quot (- n (* thousands 1000) (* hundreds 100)) 10)
        n         (mod n 10)]
    (apply str
      (flatten
        (filter #(not (nil? %))
                (list
                  (when (> thousands 0)
                    (list (word thousands) 'thousand))
                  (when (or (> hundreds 1) (and (= hundreds 1) (= thousands 0)))
                    (list (word hundreds) 'hundred))
                  (when (and (= hundreds 1) (> thousands 0))
                    'hundred)
                  (when (and (some #(> % 0) [thousands hundreds]) (some #(> % 0) [tens n]))
                    'and)
                  (when (= tens 1)
                    (word (+ (* 10 tens) n)))
                  (when (> tens 1)
                    (word (* tens 10)))
                  (when (and (not= tens 1) (> n 0))
                    (word n)))))))) 

(defn answer []
  (reduce + 0 (map #(count (say %)) (range 1 1001))))