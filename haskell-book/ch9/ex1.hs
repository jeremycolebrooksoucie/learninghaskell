toWord :: [Char] -> [[Char]]
toWord = splitOn ' '


myLines :: String -> [String]
myLines = splitOn '\n'



splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ []    = [] 
splitOn c text = chunk : (splitOn c . dropWhile (== c) $ rest)
    where stripped      = dropWhile (== c) text
          (chunk, rest) = break (== c) stripped 