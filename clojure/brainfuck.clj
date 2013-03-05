;; ------------------------------------------------------------------------
;;  Simple Brainfuck interpreter in Clojure.
;; ------------------------------------------------------------------------

(ns brainfuck
  (:import jline.Terminal)
  (:use clojure.test))

;; ------------------------------------------------------------------------
;;  Unit tests.
;; ------------------------------------------------------------------------

(deftest test-instr-gt
  (is (instr-gt nil [0 1 2] 0 0) [[0 1 2] 1 1]))

(deftest test-instr-lt
  (is (instr-lt nil [0 1 2] 0 0) [[0 1 2] 1 -1]))

(deftest test-instr-plus
  (is (= (instr-plus nil [0 1 2] 0 0) [[1 1 2] 1 0]))
  (is (= (instr-plus nil [0 1 2] 0 1) [[0 2 2] 1 1])))

(deftest test-instr-minus
  (is (= (instr-minus nil [0 1 2] 0 0) [[-1 1 2] 1 0]))
  (is (= (instr-minus nil [0 1 2] 0 1) [[0 0 2] 1 1])))

(deftest test-instr-period
  (is (= (instr-period nil [0 1 2] 0 0) [[0 1 2] 1 0])))

(deftest test-instr-opening-bracket
  (is (= (instr-opening-bracket "??]" [0 1 2] 0 0) [[0 1 2] 3 0])) 
  (is (= (instr-opening-bracket "??]" [1 2 3] 0 0) [[1 2 3] 1 0])))

(deftest test-instr-closing-bracket
  (is (= (instr-closing-bracket "[??" [0 1 2] 2 0) [[0 1 2] 3 0]))
  (is (= (instr-closing-bracket "[??" [1 2 3] 2 0) [[1 2 3] 1 0])))

;; ------------------------------------------------------------------------
;;  Main program.
;; ------------------------------------------------------------------------

(defn- positions
  "Returns the indices where sequence contains what."
  [what sequence]
  (loop [acc []
         s   sequence
         i   0]
    (if (empty? s)
      acc
      (recur (if (= (first s) what)
               (conj acc i)
               acc)
             (rest s)
             (inc i)))))

(defn- instr-gt
  "Instruction '>' that increases the data pointer."
  [prog mem ip dp]
  [mem (inc ip) (inc dp)])

(defn- instr-lt
  "Instruction '<' that decreases the data pointer."
  [prog mem ip dp]
  [mem (inc ip) (dec dp)])

(defn- instr-plus
  "Instruction '+' that increases the byte at the data pointer."
  [prog mem ip dp]
  [(flatten (conj (drop (inc dp) mem)
                  (inc (nth mem dp))
                  (take dp mem)))
   (inc ip) dp])

(defn- instr-minus
  "Instruction '-' that decreases teh byte at the data pointer."
  [prog mem ip dp]
  [(flatten (conj (drop (inc dp) mem)
                  (dec (nth mem dp))
                  (take dp mem)))
   (inc ip) dp])

(defn- instr-period
  "Instruction '.' that prints the byte at the data pointer."
  [prog mem ip dp]
  (print (char (nth mem dp)))
  [mem (inc ip) dp])

(defn- instr-comma
  "Instruction ',' that reads a byte and stores it at the data pointer."
  [prog mem ip dp]
  [(flatten (conj (drop (inc dp) mem)
                  (.readCharacter (Terminal/getTerminal) System/in)
                  (take dp mem)))
   (inc ip) dp])

(defn- instr-opening-bracket
  "Instruction '[' that jumps to the next ']' if the byte at the data
   pointer is zero."
  [prog mem ip dp]
  [mem
   (inc (if (zero? (nth mem dp))
          (first (filter #(> % ip) (positions \] prog)))
          ip))
   dp])

(defn- instr-closing-bracket
  "Instruction ']' that jumps to the previous '[' if the byte at the
   data pointer is non-zero."
  [prog mem ip dp]
  [mem
   (inc (if-not (zero? (nth mem dp))
          (first (filter #(< % ip) (reverse (positions \[ prog))))
          ip))
   dp])

(defn- step
  "Executes one instruction of program prog at address ip.  Returns
   the next [mem ip dp] on success, nil otherwise."
  [prog mem ip dp]
  (when (< ip (count prog))
    (case (nth prog ip)
      \> (instr-gt              prog mem ip dp)
      \< (instr-lt              prog mem ip dp)
      \+ (instr-plus            prog mem ip dp)
      \- (instr-minus           prog mem ip dp)
      \. (instr-period          prog mem ip dp)
      \, (instr-comma           prog mem ip dp)
      \[ (instr-opening-bracket prog mem ip dp)
      \] (instr-closing-bracket prog mem ip dp)
      nil)))

(defn brainfuck
  "Runs a Brainfuck program."
  [program]
  (loop [mem (vec (repeat 30000 0))
         ip  0
         dp  0]
    (let [[mem ip dp :as state] (step program mem ip dp)]
      (when state
        (recur mem ip dp)))))

(defn hello-world
  "Sample Brainfuck program that prints 'Hello World!'."
  []
  (brainfuck (str "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++"
                  "++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+"
                  ".>.")))
