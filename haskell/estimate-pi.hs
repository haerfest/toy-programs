{- Estimate the value of pi by imagining throwing darts at a quart circle
   with radius 1.  The ratio of the number of darts that hit the quart circle
   and those that miss, gives us a rough indication of a quarter of pi.

   Example usage:

   % ghc --make -O3 -threaded estimate-pi.hs
   % time ./estimate-pi 1000000 +RTS -N
   3.14466
   ./estimate-pi 1000000 +RTS -N  6,06s user 0,10s system 185% cpu 3,319 total
-}

import Control.Monad
import Control.Monad.Parallel
import System.Environment
import System.Random

-- |Throws one dart randomly at the square circle and returns
-- |whether the dart hit the quarter circle.
throwOne :: IO Bool
throwOne = do
  let range = (0.0, 1.0) :: (Double, Double)
  x <- getStdRandom $ randomR range
  y <- getStdRandom $ randomR range
  let radius = sqrt (x^2 + y^2)
  return (radius <= 1.0)

-- |Returns x incremented by one if a predicate holds, x otherwise.
incIf :: Int -> Bool -> Int
incIf x p = x + fromEnum p

-- |Recursively counts the number of darts that hits the quarter circle.
-- |The ($!) operator is only there to force eager evaluation of the
-- |accumulator calculation, to prevent building up too many chunks for
-- |large values of n (e.g. 1,000,000) and blowing the stack.
countHits :: Int -> Int -> IO Int
countHits 0 acc = return acc
countHits n acc = throwOne >>= ($!) (countHits (n - 1)) . incIf acc

-- |Returns the estimate of pi based on the number of darts thrown at the
-- |quarter circle and the number of darts that hit.
unratio :: Int -> Int -> Double
unratio throws hits = 4.0 * fromIntegral hits / fromIntegral throws

-- |Returns the estimate of pi for a given number of dart throws.
estimatePi :: Int -> IO Double
estimatePi n = countHits n 0 >>= return . unratio n

-- |Dual-core version of estimatePi.
estimatePiPar :: Int -> IO Double
estimatePiPar :: Int -> IO Double
estimatePiPar n = do
  let n1 = quot n 2
      n2 = n - n1
  child <- forkExec $ countHits n2 0
  h1 <- countHits n1 0
  h2 <- child
  return $ unratio n (h1 + h2)
  
-- |Prints an estimate of pi for the number of dart throws specified as
-- |the single command-line argument.
main :: IO ()
main = getArgs >>= estimatePiPar . read . head >>= putStrLn . show
