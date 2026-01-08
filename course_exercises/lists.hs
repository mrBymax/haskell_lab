-- 1. Remove the elements (any type) in even position
-- removeEven [1,2,3] -> [1,3]
removeEven :: [a] -> [a]
removeEven [] = []
removeEven [x] = [x]
removeEven (x : _ : xs) = x : removeEven xs

-- 2. Sum the elements in odd positions in a list
-- sumOdd [1,2,3] -> 4
sumOdd :: [Int] -> Int
sumOdd [] = 0
sumOdd [x] = x
sumOdd (x : _ : xs) = x + sumOdd xs

-- We can reuse the above function
sumOdd2 xs = sum (removeEven xs)

-- 3. Polimorphic QuickSort (on ordinable data types)
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let left = quicksort [a | a <- xs, a <= x]
      right = quicksort [a | a <- xs, a > x]
   in left ++ [x] ++ right

-- 4. Find the two smallest odd numbers in a list
-- minOdd [2,3,4,6,8,7,5] -> [3,5]
-- minOdd :: [Int] -> [Int]
-- take the first two of a sorted array containing only odd numbers
minOdd xs = take 2 (quicksort (filter (\n -> n `mod` 2 == 1) xs))
