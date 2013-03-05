import Data.Char
import Data.List

noInput :: String
noInput = ""

-- |Returns a sequence with the element at a particular index replaced by
-- |applying the function f to that element.
change seq index f = take index seq ++ [f (seq !! index)] ++ drop (index + 1) seq

-- |Returns the indices of the items in a sequence that satisfy a predicate.
findPred pred item seq = filter pred $ elemIndices item seq

-- |Returns the index of the next occurrence of item in a sequence.
findNext item seq index = minimum $ findPred (> index) item seq

-- |Returns the index of the previous occurrence of item in a sequence.
findPrev item seq index = maximum $ findPred (< index) item seq

-- |Executes one instruction, returning the next state of the world.
step prog mem ip dp input output =
  case prog !! ip of
    '>' -> (mem, ip + 1, dp + 1, input, output)
    '<' -> (mem, ip + 1, dp - 1, input, output)
    
    '+' -> (change mem dp (+ 1),        ip + 1, dp, input, output)
    '-' -> (change mem dp (subtract 1), ip + 1, dp, input, output)
    
    '.' -> (mem, ip + 1, dp, input, output ++ [chr $ mem !! dp])
    ',' -> (change mem dp (\_ -> ord $ head input), ip + 1, dp, tail input, output)
    
    '[' -> if mem !! dp == 0 then
             (mem, findNext ']' prog ip + 1, dp, input, output)
           else
             (mem, ip + 1, dp, input, output)
    ']' -> if mem !! dp /= 0 then
             (mem, findPrev '[' prog ip + 1, dp, input, output)
           else
             (mem, ip + 1, dp, input, output)

-- |Runs a Brainfuck program, returning its output.
run prog mem ip dp input output
  | ip < length prog = run prog mem' ip' dp' input' output'
  | otherwise        = output
  where (mem', ip', dp', input', output') = step prog mem ip dp input output

-- |Sets up a Brainfuck environment and runs a program, returning its output.
brainfuck prog input = run prog mem ip dp input output
  where mem = take 30000 (repeat 0)
        ip = 0
        dp = 0
        output = ""

-- |A Brainfuck program that returns "Hello World!\n".
helloWorld = brainfuck "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." noInput
