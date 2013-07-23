(reduce #(+ %1 (Character/digit %2 10)) 0 (str (reduce *' 1 (range 1 101))))
