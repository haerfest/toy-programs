(import '(java.util Calendar GregorianCalendar))

(defn answer []
  (count (filter #(= % 1) (for [y (range 1901 2001) m (range 12)]
                            (. (GregorianCalendar. y m 1) get Calendar/DAY_OF_WEEK)))))
