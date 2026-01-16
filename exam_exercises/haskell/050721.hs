import Data.List (inits, maximumBy, subsequences, transpose)
import Data.Ord (comparing)

-- | Checks if a list is a palindrome by comparing it to its reverse.
palindrome :: (Eq a) => [a] -> Bool
palindrome list = list == reverse list

-- | Checks if a matrix is row-symmetric. This is true if every row in the matrix is a palindrome.
rowsymmetric :: (Eq a) => [[a]] -> Bool
rowsymmetric = all palindrome -- being row-symmetric means that each row must be palindrome

-- | Checks if a matrix is column-symmetric.
-- This is true if the list of rows is a palindrome itself (i.e., the first row is identical to the last, the second to the second-to-last, etc.).
colsymmetric :: (Eq a) => [[a]] -> Bool
colsymmetric = palindrome -- given matrix = [r1 r2 r3 ... rn] we have that r1 must be equal to rn, r2 = r(n-1), etc.

-- | Finds the longest palindromic subsequence in a list.
-- The strategy is to generate all subsequences, filter them to keep only the palindromes,
-- and then find the one with the maximum length.
longestPalindromicSubsequence :: (Eq a) => [a] -> [a]
longestPalindromicSubsequence [] = []
longestPalindromicSubsequence list =
  let allSubsequences = subsequences list
      palindromes = filter palindrome allSubsequences
   in maximumBy (comparing length) palindromes
