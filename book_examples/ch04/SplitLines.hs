splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
   in pre : case suf of
        ('\r' : '\n' : rest) -> splitLines rest
        ('\r' : rest) -> splitLines rest
        ('\n' : rest) -> splitLines rest
        _ -> []

isLineTerminator c = c == '\r' || c == '\n'

-- Stitch lines back together
fixLines :: String -> String
fixLines input = unlines (splitLines input) -- unlines: concatenates each string with '\n'
