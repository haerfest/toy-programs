{- http://www.haskell.org/haskellwiki/Monad/ST -}

module MonadSTExample (main) where

import Control.Monad.ST (runST)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Control.Monad (forM_)

sumST :: Num a => [a] -> a
sumST xs = runST $ do
  n <- newSTRef 0
  forM_ xs $ \x -> modifySTRef n (+x)
  readSTRef n

main :: IO ()
main = putStrLn $ show $ sumST [1..100]
