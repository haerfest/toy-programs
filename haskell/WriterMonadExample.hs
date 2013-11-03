module WriterMonadExample (main) where

import Control.Monad.Writer (Writer, runWriter, tell)

-- |Each function we call in sequence writes what it's doing to a log, which we
-- define to be a list of strings.
type Log = [String]

-- |Return a value and write it to the log.
imagine :: Writer Log Int
imagine = do
  let imagination = 42
  tell ["I imagined " ++ (show imagination)]
  return $ imagination

-- |Return double the argument, and write it to the log.
doubleImagination :: Int -> Writer Log Int
doubleImagination x = do
  let doubled = x + x
  tell ["I doubled the imagination of " ++ (show x) ++ " to " ++ (show doubled)]
  return $ doubled

-- |Return whether the argument is even, and write it to the log.
isImaginationEven :: Int -> Writer Log Bool
isImaginationEven x = do
  tell ["The imagination " ++ (show x) ++ " is even: " ++ (show $ even x)]
  return $ even x

-- |Print the final result and the log of running three functions in sequence.
-- Its output is:
-- >The final imagination is even: True
-- >I imagined 42
-- >I doubled the imagination of 42 to 84
-- >The imagination 84 is even: True
main :: IO ()
main = do
  let (isEven, log) = runWriter (imagine >>= doubleImagination >>= isImaginationEven)
  putStrLn $ "The final imagination is even: " ++ (show isEven)
  mapM_ putStrLn log
  
  
