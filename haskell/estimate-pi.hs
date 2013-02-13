{- Estimate the value of pi by imagining throwing darts at a square circle
   with radius 1.  The ratio of the number of darts that hit the square circle
   and those that miss, gives us a rough indication of a quarter of pi.

   Example usage:

   % ghc --make -O3 estimate-pi.hs
   % ./estimate-pi 1000000
   3.143564
-}

import Control.Monad
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
countHits :: IO Bool -> Int -> Int -> IO Int
countHits fn 0 acc = return acc
countHits fn n acc = fn >>= ($!) (countHits fn (n - 1)) . incIf acc

-- |Returns the estimate of pi based on the number of darts thrown at the
-- |quarter circle and the number of darts that hit.
unratio :: Int -> Int -> Double
unratio throws hits = 4.0 * fromIntegral hits / fromIntegral throws

-- |Returns the estimate of pi for a given number of dart throws.
estimatePi :: Int -> IO Double
estimatePi n = countHits throwOne n 0 >>= return . unratio n

-- |Prints an estimate of pi for the number of dart throws specified as
-- |the single command-line argument.
main :: IO ()
main = getArgs >>= estimatePi . read . head >>= putStrLn . show
