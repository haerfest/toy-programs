module ReaderMonadExample (main) where

import Control.Monad.Reader (Reader, ask, runReader)

-- |The environment has a dedicated type for readability.
type Env = Int

-- |Return the environment squared.
squareEnv :: Reader Env Int
squareEnv = do
  e <- ask
  return $ e * e

-- |Return the environment increased by x.
addToEnv :: Int -> Reader Env Int
addToEnv x = do
  e <- ask
  return $ e + x

-- |The environment.
env = 10 :: Env

-- |Print the result of chaining two functions, all of which refer to the same
-- environment.  The result is env^2 + env.
main :: IO ()
main = do
  let result = runReader (squareEnv >>= addToEnv) env
  putStrLn $ show result
