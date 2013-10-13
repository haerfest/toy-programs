{- 99 problems in Haskell
   http://www.haskell.org/haskellwiki/99_questions -}

import Data.List (group)
import System.Random

-- P01. Find the last element of a list.
myLast = last

-- P02. Find the last but one element of a list.
myButLast = last . init

-- P03. Find the K'th element of a list.  The first element in the list is
--      number 1.
elementAt xs k = xs !! (k - 1)

-- P04. Find the number of elements of a list.
myLength = length

-- P05. Reverse a list.
myReverse = reverse

-- P06. Find out whether a list is a palindrome.  A palindrome can be read
--      forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs | head xs == last xs = isPalindrome middle
                | otherwise          = False
  where middle = init $ tail xs

-- P07. Flatten a nested list structure.  Transform a list, possibly holding
--      lists as elements into a `flat' list by replacing each list with its
--      elements (recursively).
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = concat $ map flatten xs

-- P08. Eliminate consecutive duplicates of list elements.  If a list contains
--      repeated elements they should be replaced with a single copy of the
--      element.  The order of the elements should not be changed.
compress :: Eq a => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:y:zs) | x == y    = compress (y:zs)
                  | otherwise = [x] ++ compress (y:zs)

-- P09. Pack consecutive duplicates of list elements into sublists.  If a list
--      contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack = group

-- P10. Run-length encoding of a list.  Use the result of problem P09 to
--      implement the so-called run-length encoding data compression method.
--      Consecutive duplicates of elements are encoded as lists (N E) where N is
--      the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map f $ pack xs
  where f xs = (length xs, head xs)

-- P11. Modified run-length encoding.  Modify the result of problem 10 in such
--      a way that if an element has no duplicates it is simply copied into the
--      result list.  Only elements with duplicates are transferred as (N E)
--      lists.
data Occ a = Single a | Multiple Int a
           deriving (Show)
encodeModified :: Eq a => [a] -> [Occ a]
encodeModified xs = map f $ pack xs
  where
    f [x] = Single x
    f xs  = Multiple (length xs) (head xs)

-- P12. Decode a run-length encoded list.  Given a run-length code list
--      generated as specified in problem 11.  Construct its uncompressed
--      version.
decodeModified :: [Occ a] -> [a]
decodeModified xs = concat $ map f xs
  where
    f (Single x)     = [x]
    f (Multiple n x) = take n $ repeat x

-- P13. Run-length encoding of a list (direct solution).  Implement the
--      so-called run-length encoding data compression method directly.  I.e.
--      don't explicitly create the sublists containing the duplicates, as in
--      problem 9, but only count them.  As in problem P11, simplify the result
--      list by replacing the singleton lists (1 X) by X.
encodeDirect :: Eq a => [a] -> [Occ a]
encodeDirect [] = []
encodeDirect (x:xs) = f xs (Single x)
  where
    f :: Eq a => [a] -> Occ a -> [Occ a]
    f [] occ                         = [occ]
    f (x:xs) (Single y)     | x == y = f xs (Multiple 2 x)
    f (x:xs) (Multiple n y) | x == y = f xs (Multiple (n + 1) x)
    f (x:xs) occ                     = [occ] ++ f xs (Single x)

-- P14. Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli []  = []
dupli (x:xs) = x : x : dupli xs

-- P15. Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli _ 0  = []
repli [] _ = []
repli (x:xs) n | n > 0 = x : repli [x] (n - 1) ++ repli xs n

-- P16. Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n | n > 0 = f xs n
  where
    f :: [a] -> Int -> [a]
    f [] _ = []
    f (x:xs) 1 = f xs n
    f (x:xs) m = x : f xs (m - 1)

-- P17. Split a list into two parts; the length of the first part is given.  Do
--      not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split xs n = splitAt n xs

-- P18. Extract a slice from a list.  Given two indices, i and k, the slice is
--      the list containing the elements between the i'th and k'th element of
--      the original list (both limits included). Start counting the elements
--      with 1.
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k - i + 1) $ drop (i - 1) xs

-- P19. Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs n | n >= 0 = drop n xs ++ take n xs
            | n < 0  = drop m xs ++ take m xs
  where m = length xs + n

-- P20. Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (elementAt xs n, take (n - 1) xs ++ drop n xs)

-- P21. Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs k = take m xs ++ [x] ++ drop m xs
  where m = k - 1

-- P22. Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range i k = [i..k]

-- P23. Extract a given number of randomly selected elements from a list.
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  gen <- getStdGen
  return $ f [] n gen
  where
    f ys m gen | m == 0 = ys
               | m > 0  = f (ys ++ [elementAt xs k]) (m - 1) g
      where (k, g) = randomR (1, length xs) gen
