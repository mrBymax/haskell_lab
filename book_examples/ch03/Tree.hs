-- Let's define a binary tree to see how recursive data type works in Haskell
data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

-- We can see a binary tree either as a node with two children or (|) an empty value
-- Let's analyse a difference between Java class and Haskell class
-- class Example
-- {
--     static Tree<String> simpleTree()
--     {
-- 	return new Tree<String>(
--             "parent",
-- 	    new Tree<String>("left leaf", null, null),
-- 	    new Tree<String>("right leaf", null, null));
--     }
-- }

simpleTree = Node "parent" (Node "left child" Empty Empty) (Node "right child" Empty Empty) -- Notice the absence of `null` data type.

-- data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving (Show)
