myDumbExample xs =
  if length xs > 0 -- it's a lot of useless work because we're just interested in if the list is empty
    then head xs
    else 'Z'

mySmartExample xs =
  if not (null xs) -- a lot smarter
    then head xs
    else 'Z'

myOtherExample (x : _) = x
myOtherExample [] = ['Z']
