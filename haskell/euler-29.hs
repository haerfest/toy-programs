import Data.List (sort)

-- precondition: list is sorted
nub []   = []
nub (x:xs) = x : nub rest
  where
    rest = dropWhile ((==) x) xs

answer n = length $ nub $ sort [a^b | a <- [2..n], b <- [2..n]]
