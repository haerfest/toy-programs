module StateMonadExamples (main) where

import Control.Monad.State (State, get, put, runState)

-- |We keep a step count as our state.  Each function we call increments it by
-- one.
type StepCount = Int

-- |Increment the step count state and return three.
three :: State StepCount Int
three = do
  stepCount <- get
  put $ stepCount + 1
  return 3

-- |Increment the step count state and its argument.
increment :: Int -> State StepCount Int
increment x = do
  stepCount <- get
  put $ stepCount + 1
  return $ x + 1

-- |Increment the step count state and return the sum of its arguments.
add :: Int -> Int -> State StepCount Int
add x y = do
  stepCount <- get
  put $ stepCount + 1
  return $ x + y

-- |Run three functions in sequence and print the number of steps performed and
-- the final outcome.
-- Its output is:
-- >I performed 3 steps and got a sum of 14
main :: IO ()
main = do
  let (sum, stepCount) = runState (three >>= increment >>= add 10) 0
  putStrLn $ "I performed " ++ (show stepCount) ++ " steps and got a sum of " ++ (show sum)
