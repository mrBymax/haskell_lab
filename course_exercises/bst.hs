import Control.Applicative ((<|>))

-- Definition of the Binary Search Tree ADT
data BST a
  = Void
  | Node
      { val :: a,
        left, right :: BST a
      }
  deriving (Eq, Ord, Read, Show)

type Vector a = [a]

-- Example tree for checking functions
exampleTree :: BST Int
exampleTree =
  Node
    10
    (Node 5 Void Void)
    (Node 15 (Node 12 Void Void) Void)

-- 1. Sum nodes
-- sumnodes exampleTree -> 42
sumnodes :: (Num a) => BST a -> a
sumnodes Void = 0
sumnodes (Node val left right) = val + sumnodes left + sumnodes right

-- 2. Sum odd nodes
-- sumnodes exampleTree -> 20
sumoddnodes :: (Integral a) => BST a -> a -- Integral is necessary because Float would not be included in `Num`
sumoddnodes Void = 0
sumoddnodes (Node val left right) =
  (if odd val then val else 0) + sumoddnodes left + sumoddnodes right

-- 3. samesums: checks if (given a list of trees), their sums are equal
-- samesums [exampleTree, exampleTree] -> True
samesums :: (Num a, Eq a) => [BST a] -> Bool
samesums [] = True
samesums trees =
  let allEqual :: (Eq a) => [a] -> Bool
      allEqual [] = True
      allEqual (x : xs) = all (== x) xs
      sums = map sumnodes trees
   in allEqual sums

-- 4. bstElem: a function that checks if there's a specific element in a bst
-- bstElem 10 exampleTree -> True
bstElem :: (Ord a) => a -> BST a -> Bool
bstElem _ Void = False
bstElem x (Node val left right)
  | x == val = True
  | x < val = bstElem x left
  | x > val = bstElem x right

-- 5. bstInsert: a function that inserts an element in the correct position of a bst
-- bstInsert 15 exampleTree  -> newTree
bstInsert :: (Ord a, Eq a) => a -> BST a -> BST a
bstInsert x Void = Node x Void Void
bstInsert x (Node val left right)
  | x == val = Node val left right
  | x < val = Node val (bstInsert x left) right
  | x > val = Node val left (bstInsert x right)

-- 6. bst2List: a function that returns a sorted list of bst elements
-- bst2List exampleTree -> [5,10,12,15]
bst2List :: BST a -> [a]
bst2List Void = []
bst2List (Node val left right) = bst2List left ++ [val] ++ bst2List right

-- 7. sort: a function that sorts a list
-- sort [5,2,3,4,1] -> [1,2,3,4,5]
sort :: (Ord a) => Vector a -> Vector a
sort [] = []
sort [x] = [x]
sort list =
  let (left, right) = splitAt (div (length list) 2) list
      merge :: (Ord a) => [a] -> [a] -> [a]
      merge a [] = a
      merge [] b = b
      merge (x : xs) (y : ys) =
        if x <= y
          then x : merge xs (y : ys)
          else y : merge (x : xs) ys
   in merge (sort left) (sort right)

-- 8. filtertree: a function that builds a sorted list of elements that satisfy a predicate
-- filtertree (<= 10) exampleTree -> [5, 10]
filtertree :: (a -> Bool) -> BST a -> [a]
filtertree p = filter p . bst2List

-- 9. annotate: build a new BST in which each node contains a pair (val, height)
-- annotate exampleTree -> Node (10,3) (Node (5,1) Void Void) (Node (15,2) (Node (12,1) Void Void) Void)
annotate :: BST a -> BST (a, Int)
annotate = fst . annotate'

-- annotate helper function to create the new nodes in the tree
annotate' :: BST a -> (BST (a, Int), Int)
annotate' Void = (Void, 0)
annotate' (Node val left right) =
  let (newLeft, lHeight) = annotate' left
      (newRight, rHeight) = annotate' right
      h = 1 + max lHeight rHeight
      newNode = Node (val, h) newLeft newRight
   in (newNode, h)

-- 10. almostBalanced: check if a tree is almost balanced
-- almostBalanced exampleTree -> True
almostBalanced :: BST a -> Bool
almostBalanced = fst . balanceWithHeight

-- helper that returns a pair (isAlmostBalanced, height)
balanceWithHeight :: BST a -> (Bool, Int)
balanceWithHeight Void = (True, 0)
balanceWithHeight (Node _ left right) =
  let (isLeftBalanced, lHeight) = balanceWithHeight left
      (isRightBalanced, rHeight) = balanceWithHeight right
   in (isLeftBalanced && isRightBalanced && abs (lHeight - rHeight) <= 1, 1 + max lHeight rHeight)

data WBST a = WVoid | WNode a Int (WBST a) (WBST a) deriving (Eq, Ord, Read, Show)

-- for testing
exampleWBST :: WBST Int
exampleWBST = WNode 10 3 (WNode 5 1 WVoid WVoid) (WNode 15 1 WVoid WVoid)

size :: WBST a -> Int
size WVoid = 0
size (WNode _ w _ _) = w

-- 11. insert into a weighted bst
-- insert 4 exampleWBST -> WNode 10 4 (WNode 5 2 (WNode 4 1 WVoid WVoid) WVoid) (WNode 15 1 WVoid WVoid)
insert :: (Ord a, Eq a) => a -> WBST a -> WBST a
insert x WVoid = WNode x 1 WVoid WVoid
insert x (WNode v w l r)
  | x == v = WNode v w l r
  | x < v =
      let newLeft = insert x l
          weight = size newLeft + size r + 1
       in WNode v weight newLeft r
  | x > v =
      let newRight = insert x r
          weight = size newRight + size l + 1
       in WNode v weight l newRight

-- 12. diff2next: given a BST, we build a BST of pairs in which the first element is preserved and the second is (Just) the difference with the next value in the BST oppure Nothing if they're equal
-- diff2next Node 4 Void (Node 7 (Node 5 Void Void) Void) -> Node (4,Just 1) Void (Node (7,Nothing) (Node (5,Just 2) Void Void) Void).
-- idea: reverse in-order traversal
diff2next :: (Num a, Ord a) => BST a -> BST (a, Maybe a)
diff2next bst = helper bst Nothing

findMin :: BST a -> a -- must be non empty
findMin (Node v Void _) = v
findMin (Node _ l _) = findMin l

helper :: (Num a, Ord a) => BST a -> Maybe a -> BST (a, Maybe a)
helper Void _ = Void -- an empty tree remains empty
helper (Node val left right) parentSuccessor =
  let newRight = helper right parentSuccessor
      currentSuccessor = case right of
        Void -> parentSuccessor -- we cannot find any other successor
        r -> Just (findMin r)
      diff = fmap (\succVal -> succVal - val) currentSuccessor
      newLeft = helper left (Just val) -- could not be greater if we scan left
   in Node (val, diff) newLeft newRight

-- 13. levelOrderVisit
-- levelOrderVisit exampleTree -> [10, 5, 15, 12]
levelOrderVisit :: (Eq a, Ord a) => BST a -> Vector a
levelOrderVisit Void = []
levelOrderVisit bst = concat (levels [bst])
  where
    levels :: [BST a] -> [[a]]
    levels [] = []
    levels nodes =
      let currentVals = [val | Node val _ _ <- nodes]
          children = concat [[left, right] | Node _ left right <- nodes]
       in if null currentVals
            then []
            else currentVals : levels children

-- post-order travaersal fold implementation
fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
-- fold function base_value tree = f val (fold f base_value left) (fold f base_value right)
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

-- 14. treeheight with fold
-- treeheight exampleTree -> 3
treeheight :: (Ord a) => BST a -> Int
treeheight = fold (\_ lHeight rHeight -> 1 + max lHeight rHeight) 0

-- skipped 15 and 16 because they were variants of already-solved exercises
-- 17. maxDiameter: given a list of BSTs determine the absolute max diameter
-- maxDiameter [exampleTree] -> 4
maxDiameter :: (Eq a, Ord a) => [BST a] -> Int
maxDiameter [] = 0
maxDiameter bsts = maximum (map diameter bsts)

-- Version 1: inefficient because we call diameter on the same subtrees multiple times
slowdiameter :: BST a -> Int
slowdiameter Void = 0
slowdiameter (Node _ left right) =
  let leftSubtree = slowdiameter left
      rightSubtree = slowdiameter right
      path = leftSubtree + rightSubtree + 1 -- through root
   in max path (max leftSubtree rightSubtree)

-- Version 2:
diameter :: BST a -> Int
diameter bst = snd (heightAndDiameter bst)

-- a faster implementation would calculate both hight and diameter at the same time
heightAndDiameter :: BST a -> (Int, Int)
heightAndDiameter Void = (0, 0)
heightAndDiameter (Node _ left right) =
  let (leftHeight, leftDiameter) = heightAndDiameter left
      (rightHeight, rightDiameter) = heightAndDiameter right
      currentHeight = 1 + max leftHeight rightHeight
      path = leftHeight + rightHeight + 1
      currentDiameter = max path (max leftDiameter rightDiameter)
   in (currentHeight, currentDiameter)

-- 18. isBST: check if a given tree is a bst
-- isBST exampleTree -> True
isBST :: (Ord a) => BST a -> Bool
isBST Void = True
-- we need a richer DS that tells us:
-- 1. is this a valid BST?
-- 2. it's min value (Maybe a)
-- 3. it's max value (Maybe a)
-- at each step the result should be a tuple (Bool, Maybe a, Maybe a)
isBST bst = fst_of_3 (fold isBST_helper (True, Nothing, Nothing) bst)
  where
    fst_of_3 (b, _, _) = b
    isBST_helper val (isBST_left, minVal_left, maxVal_left) (isBST_right, minVal_right, maxVal_right) =
      let isValid =
            isBST_left
              && isBST_right
              && all (< val) maxVal_left
              && all (> val) minVal_right
          newMin = minVal_left <|> Just val
          newMax = maxVal_right <|> Just val
       in (isValid, newMin, newMax)

-- please, note the usage of the opearator <|> instead of:
-- new_min = case l_min of
--            Just left_minimum -> Just left_minimum
--            Nothing           -> Just val
