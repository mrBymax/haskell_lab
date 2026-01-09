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

-- 5. Build a list of pairs of ints where the first element is preserved and the second element is the sum of the remaining
-- elements in the list
-- build [1,2,3,4] --> [[1,9],[2,7],[3,4],[4,0]]
build :: [Int] -> [(Int, Int)]
build [] = []
build (x : xs) = (x, sum xs) : build xs

build2 :: [Int] -> [(Int, Int)]
build2 list = go list (sum list)
  where
    go :: [Int] -> Int -> [(Int, Int)]
    go [] _ = []
    go (x : xs) currentSum =
      let remainingSum = currentSum - x
       in (x, remainingSum) : go xs remainingSum

-- 6. Build a list of pairs of ints where the first element is preserved and the second element is the sum of the remaining
-- elements in the list from the left
-- build [1,2,3,4] --> [[1,0],[2,1],[3,3],[4,6]]
build3 :: [Int] -> [(Int, Int)]
build3 list = go list 0
  where
    go :: [Int] -> Int -> [(Int, Int)]
    go [] _ = []
    go (x : xs) currentAcc =
      let totalSum = currentAcc + x
       in (x, currentAcc) : go xs totalSum

-- 7. Modify a list in which the min element is subtracted to each element in the list
-- shiftToZero [1,2,3,4] -> [0,1,2,3]
shiftToZero :: [Int] -> [Int]
shiftToZero list =
  let min = minimum list
   in [x - min | x <- list]
