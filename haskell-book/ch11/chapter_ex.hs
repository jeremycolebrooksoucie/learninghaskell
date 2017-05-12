import Data.Char

-- 1 a

-- 2 c 

-- 3 b

-- 4 c 

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _  = True
isSubsequenceOf _  [] = False
isSubsequenceOf m @ (x:xs) (y:ys)
                    | x == y = isSubsequenceOf xs ys
                    | otherwise = isSubsequenceOf m ys

capitalizeWord (x : xs) | x == ' '   = x : capitalizeWord xs
                        | otherwise =  toUpper x : xs 
capitalizeWord []       = []

--capitalizeParagraph (x : y : xs) 
--                        | x == '.' = x : toUpper y : capitalizeParagraph xs
--                        | otherwise = x : capitalizeParagraph (y : xs)
--capitalizeParagraph [x] = [x]
--capitalizeParagraph []  = []

capitalizeParagraph = cap . capitalizeWord
    where cap (x : xs)
            | x == '.' = x : (cap $ capitalizeWord $ xs)
            | otherwise = x : cap xs
          cap [] = []
