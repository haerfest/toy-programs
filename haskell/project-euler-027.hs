import Data.List (foldl1')
import Data.Numbers.Primes (primes)

data Tuple = Tuple Int Int Int
           deriving Show
instance Eq Tuple where
  (Tuple _ _ a) == (Tuple _ _ b) = a == b
instance Ord Tuple where
  (Tuple _ _ a) `compare` (Tuple _ _ b) = a `compare` b

coefficients :: [(Int, Int)]
coefficients = [(a, b) | a <- [-1000..1000], b <- [-1000..1000]]

isPrime n = head (dropWhile (< n) primes) == n

tuple (a, b) = Tuple a b n
  where
    n = length $ takeWhile isPrime [n^2 + a * n + b | n <- [0..]]

solution = a * b
  where
    Tuple a b _ = foldl1' max $ map tuple coefficients

main = putStrLn $ show solution
