import Data.List (inits, intersect, maximumBy, tails)
import Data.Ord (comparing)

-- | Checks if l2 is a subset of l1.
-- NOTE: This implementation does not check for a contiguous sublist. It checks if all elements of l2 are present in l1, regardless of order or contiguity.
issublist :: [Int] -> [Int] -> Bool
issublist l1 l2 = intersect l1 l2 == l2

-- | Checks if l1 is a non-contiguous subsequence of l2.
-- It recursively consumes l2, matching elements of l1 in order.
issublist2 :: [Int] -> [Int] -> Bool
issublist2 [] [] = True
issublist2 _ [] = False
issublist2 [] _ = True
issublist2 (x : xs) (y : ys)
  | x == y = issublist2 xs ys
  | otherwise = issublist2 (x : xs) ys

-- | Checks if a list is a palindrome by comparing it to its reverse.
ispalindrome :: (Eq a) => [a] -> Bool
ispalindrome xs = xs == reverse xs

-- | Generates all non-empty contiguous sublists of a list.
-- It does this by taking all initial segments (`inits`) of all tail segments (`tails`).
sublists :: [a] -> [[a]]
sublists = concatMap (tail . inits) . tails

-- | Finds the longest contiguous palindromic sublist in a given list.
-- The strategy is to generate all sublists, filter them to keep only the palindromes,
-- and then find the one with the maximum length.
longestPalindrome :: (Eq a) => [a] -> [a]
longestPalindrome [] = []
longestPalindrome xs =
  let allSublists = sublists xs
      palindromes = filter ispalindrome allSublists
   in maximumBy (comparing length) palindromes
