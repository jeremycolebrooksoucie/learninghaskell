import Data.Char
-- 1
findUpper = filter isUpper

capitalizeFirst (c : rest) = toUpper c : rest
capitalizeFirst [] = []

capitalize = map toUpper

getCapitaledFirst = toUpper . head


myOr (True : _) = True
myOr (_ : rest) = myOr rest
myOr [] = False

myAny f = myOr . map f 

myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a

myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = 
    case f x maxRest of
        LT -> maxRest
        EQ -> maxRest
        GT -> x
    where maxRest = myMaximumBy f xs 

myMinimumBy f = myMaximumBy g 
    where g a b = case f a b of
                    LT -> GT
                    EQ -> EQ
                    GT -> LT

myMaximum = myMaximumBy compare
myMinimum = myMinimumBy compare
