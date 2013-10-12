{- 99 problems in Haskell
   http://www.haskell.org/haskellwiki/99_questions -}

import Data.List (concat, group)

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
isPalindrome xs
  | head xs == last xs  = isPalindrome middle
  | otherwise           = False
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
compress (x:y:zs)
  | x == y     = compress (y:zs)
  | otherwise  = [x] ++ compress (y:zs)

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
