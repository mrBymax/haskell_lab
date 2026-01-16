-- | Checks if an element `el` is present in a list of integers.
-- It recursively checks the head of the list and then the tail.
isin :: [Int] -> Int -> Bool
isin [] _ = False -- empty lists do not contain elements
isin (x : xs) el =
  x == el -- check if the element is the current head of the list
    || isin xs el -- call the function on the rest of the list

-- | Removes the first occurrence of an element `el` from a list.
-- It recursively traverses the list until it finds the element.
remove :: [Int] -> Int -> [Int]
remove [] _ = [] -- removing from an empty list, returns an empty list
remove (x : xs) el
  -- If the head of the list is the element, return the tail, effectively removing the head.
  | x == el = xs
  -- Otherwise, keep the head and continue searching in the tail.
  | otherwise = x : remove xs el

-- | Checks if two lists are permutations of each other.
-- The strategy is recursive:
-- 1. Ensure the lists have the same length.
-- 2. Take an element from the first list.
-- 3. Check if it's in the second list. If not, they are not permutations.
-- 4. If it is, remove it from the second list and recursively check the remaining lists.
ispermutation :: [Int] -> [Int] -> Bool
ispermutation l1 l2 | length l1 /= length l2 = False
ispermutation [] [] = True
ispermutation (x : xs) ys = isin ys x && ispermutation xs (remove ys x)

-- | Helper function: checks if a given `row` is a permutation of any of the rows in `otherRows`.
helper :: [Int] -> [[Int]] -> Bool
helper _ [] = False
helper row (first : rest) = ispermutation row first || helper row rest

-- | Checks if a matrix contains at least one row that is a permutation of another row.
-- It works by taking each row and using a helper to compare it against all subsequent rows.
isrowpermutation :: [[Int]] -> Bool
isrowpermutation [] = False
isrowpermutation [_] = False
isrowpermutation (head : tail) = helper head tail || isrowpermutation tail
