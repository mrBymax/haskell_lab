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
