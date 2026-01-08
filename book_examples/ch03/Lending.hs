-- Let's see how the where clause works:
lend amount balance =
  let reserve = 100 -- LET starts the block of the variable declarations
      newBalance = balance - amount -- each line introduces a new variable
   in if balance < reserve -- IN ends the block of variable declarations
        then Nothing
        else Just newBalance

-- Remember that inside a let block are binded expressions not values!

-- Another way to introduce variables is through the `where` clause:
lend2 amount balance =
  if amount < reserve * 0.5
    then Just newBalance
    else Nothing
  where
    reserve = 100
    newBalance = balance - amount

-- Here's an implementation with guards:
lend3 amount balance
  | amount <= 0 = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise = Just newBalance -- notice that `otherwise` is used just for readability
  where
    reserve = 100
    newBalance = balance - amount
