-- NUMBERS
-- 1. Factorial (assuming n >= 0)
factorial :: Int -> Int
factorial n
  | n < 0 = 0
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- 2. Binomial coefficient
-- binom n k = (factorial n) / ((factorial k) * (factorial (n-k)))
--           =  binom (n-1) (k-1) + binom (n-1) k

binom :: Int -> Int -> Int
binom n k
  | k < 0 || k > n = 0
  | k == 0 = 1
  | k == n = 1
  | otherwise = binom (n - 1) (k - 1) + binom (n - 1) k

-- 3. Combinations on list with given n
-- combinations n [1 2 3] -> [[1 2 3],[1 3 2], ... ]
combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) =
  let picked = map (x :) (combinations (n - 1) xs)
      skipped = combinations n xs
   in picked ++ skipped
