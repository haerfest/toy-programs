{- 99 questions in Haskell
   http://www.haskell.org/haskellwiki/99_questions -}

-- Q1. Find the last element of a list.
myLast = last

-- Q2. Find the last but one element of a list.
myButLast = last . init

-- Q3. Find the K'th element of a list.  The first element in the list is
--     number 1.
elementAt xs k = xs !! (k - 1)

-- Q4. Find the number of elements of a list.
myLength = length

-- Q5. Reverse a list.
myReverse = reverse

-- Q6. Find out whether a list is a palindrome. A palindrome can be read
--     forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs
  | head xs == last xs = isPalindrome middle
  | otherwise          = False
  where middle = init $ tail xs

-- Q7. Flatten a nested list structure.
--     Transform a list, possibly holding lists as elements into a `flat' list
--     by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = foldl (++) [] $ map flatten xs

