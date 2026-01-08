-- The `case` conduct lets us match patterns within an expression:
fromMaybe defval wrapped =
  case wrapped of
    Nothing -> defval
    Just value -> value

-- The `case` word is followed by an arbitrary expression and the
-- pattern matching is done on the result of that expression.

-- Another example:
data Fruit = Apple | Orange

betterFruit f = case f of
  "apple" -> Apple
  "orange" -> Orange

-- We can use guards even for conditional evaluation:
data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

nodesAreTheSame (Node a _ _) (Node b _ _)
  | a == b = Just a
nodesAreTheSame _ _ = Nothing
