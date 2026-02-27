data Tree a = Void | Node a [Tree a]
  deriving (Eq, Show)

testTree :: Tree Char
testTree =
  Node
    'a'
    [ Void,
      Node 'b' [],
      Node
        'c'
        [ Void,
          Node 'd' [Void, Void]
        ]
    ]

-- 1. treefold
-- treefold :: (Eq a, Show a) => (a -> [b] -> b) -> b -> Tree a -> b
treefold :: (Eq a, Show a) => (a -> [b] -> b) -> b -> Tree a -> b
treefold f z Void = z
treefold f z (Node x ts) = f x (map (treefold f z) ts)

-- 2. height
-- height testTree -> 2
height :: (Eq a, Show a) => Tree a -> Int
height = treefold f (-1)
  where
    f _ [] = 0
    f _ hs = 1 + maximum hs

-- 3. simplify
-- simplify testTree -> Node 'a' [Node 'b' [],Node 'c' [Node 'd' []]] (same as before because of no duplicates)
simplify :: (Eq a, Show a) => Tree a -> Tree a
simplify = treefold f z
  where
    z = Void
    f x ts = Node x (filter isNode ts)
    isNode = treefold (\_ _ -> True) False
