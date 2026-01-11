import Data.List (transpose)
import Distribution.Compat.CharParsing (upper)

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

-- 5. Check if a matrix is lower triangular
-- lowertriangular [[1,0,0],[2,3,0],[4,5,6]] -> True
lowertriangular :: Matrix -> Bool
lowertriangular [] = True
lowertriangular matrix = check matrix 0
  where
    check :: Matrix -> Int -> Bool
    check [] _ = True
    check (first : rest) n =
      let aboveTheDiagonal = drop (n + 1) first
          isValidRow = all (== 0) aboveTheDiagonal
       in isValidRow && check rest (n + 1)

-- 6. Check if a matrix is upper triangular
-- uppertriangular [[1,2,3],[0,4,5],[0,0,6]] -> True
uppertriangular :: Matrix -> Bool
uppertriangular [] = True
uppertriangular matrix = check matrix 0
  where
    check :: Matrix -> Int -> Bool
    check [] _ = True
    check (first : rest) n =
      let isValidRow = all (== 0) (take n first)
       in isValidRow && check rest (n + 1)

-- 7. Check if a matrix is diagonal
-- isDiagonal [[1,0,0],[0,1,0],[0,0,1]] -> True
-- could be enough to check both upper and lower triangular condition
isDiagonal :: Matrix -> Bool
isDiagonal matrix = uppertriangular matrix && lowertriangular matrix

-- 8. Check if the matrix is convergent with radius r (i.e. if the sum of the elements - diagonal sum is less than r)
-- isConvergent [[1,2,3],[4,5,6],[7,8,9]] 31 -> True
isConvergent :: Matrix -> Int -> Bool
isConvergent [] _ = True
isConvergent matrix r =
  -- total_sum - diagonal_sum < r?
  let totalSum = sum (concat matrix)
      diagonalSum = calculateDiagonalSum matrix 0 0
        where
          calculateDiagonalSum :: Matrix -> Int -> Int -> Int
          calculateDiagonalSum [] _ acc = acc
          calculateDiagonalSum (first : rest) n acc = calculateDiagonalSum rest (n + 1) (acc + first !! n)
   in (totalSum - diagonalSum) < r

-- 9. Transpose
-- mytranspose [[1,2,3],[4,5,6],[7,8,9]] -> [[1,4,7],[2,5,8],[3,6,9]]
mytranspose :: Matrix -> Matrix
mytranspose [] = []
mytranspose ([] : _) = [] -- if it's non-empty, is it a list whose first element is an empty list?
mytranspose m = map head m : mytranspose (map tail m)

-- 10. isSymmetric: a predicate that checks if a given matrix is symmetric
-- isSymmetric [[1,2,3],[4,5,6],[7,8,9]] -> False
isSymmetric :: Matrix -> Bool
isSymmetric [] = True
isSymmetric m =
  let (nRows, nCols) = matrixDim m
   in nRows == nCols && m == mytranspose m

-- 11. Multiply a n x k matrix with a k x m returning the result
-- matmultiply [[1,2],[3,4]] * [[1],[2]] -> [[5],[11]]
matmultiply :: Matrix -> Matrix -> Matrix
matmultiply [] _ = [[]]
matmultiply _ [] = [[]]
matmultiply a b =
  let dotProduct :: Vector -> Vector -> Int
      dotProduct u v = sum (zipWith (*) u v)
      colsB = mytranspose b
   in map (\rowA -> map (dotProduct rowA) colsB) a
