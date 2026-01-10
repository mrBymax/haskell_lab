import Data.List (transpose)
import GHC.Base (VecCount (Vec16))

-- define matrix type
type Matrix = [[Int]]

-- define vector type
type Vector = [Int]

-- 1. Write matrix_dim that calculates the size
-- matrixDim [[1,2,3],[4,5,6],[7,8,9]] -> (3,3)
matrixDim :: Matrix -> (Int, Int)
matrixDim [] = (0, 0)
-- check if the size of the first row is consistent with the others
matrixDim (first : rest) =
  let cols = length first
      rows = length (first : rest)
      isValid = all (\r -> length r == cols) rest
   in if isValid then (rows, cols) else (-1, -1)

-- 2. Column sum
-- colsums [[1,2,3],[4,5,6],[7,8,9]] -> [12, 15, 18]
colsums :: Matrix -> Vector
colsums = foldl1 (zipWith (+))

colsums2 :: Matrix -> Vector
colsums2 [] = []
colsums2 matrix = map sum (transpose matrix)

-- 3. Column alternate sum (with matrix input as list of rows)
-- colaltsums [[1,2,3],[4,5,6],[7,8,9]] -> [4, 5, 6]

-- The idea is to modify the matrix (is not forbidden) and sum with colsum implemented above
preprocess :: Matrix -> Matrix
preprocess [] = []
preprocess [x] = [x]
preprocess (x : y : rest) = x : map negate y : preprocess rest

colaltsums :: Matrix -> Vector
colaltsums [] = []
colaltsums matrix =
  let mat = preprocess matrix
   in foldl1 (zipWith (+)) mat

-- 4. Find min and max of each column in a matrix
-- minmax [[1,2,3],[4,5,6],[7,8,9]] -> [[1,7],[2,8],[3,9]]

minmax :: Matrix -> Matrix
minmax [] = []
minmax matrix =
  let cols = transpose matrix
      findMax :: Vector -> Vector
      findMax col = [minimum col, maximum col]
   in map findMax cols
