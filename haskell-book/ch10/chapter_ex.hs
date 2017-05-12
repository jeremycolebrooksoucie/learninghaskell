-- 1 

stops = "pbtdkg"
vowels = "aeiou"

makeFakeWords stops vowels = [ [s, v, s'] | 
                                s <- stops, s' <- stops, v <- vowels]

filterP = filter $ (== 'p') . head 

nouns = ["dog", "cat", "car", "table"]
verbs = ["pats", "runs", "drives", "drinks"]

sentences nouns verbs = [(n, v, n') |  n  <- nouns,
                                       v  <- verbs,
                                       n' <- nouns]


averageLength text = (fromIntegral . sum . map length $ splitText) 
                        / (fromIntegral . length $ splitText)
    where splitText = words text   


------------------------------------

myOr :: [Bool] -> Bool
myOr = foldr (||) False 

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f  

myElem :: Eq a => a -> [a] ->  Bool
myElem a = myAny (==a) 

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b ) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a

myMaximumBy _ [] = undefined
myMaximumBy f (a : as) = foldr myMax a as
    where myMax a b = case f a b of
                        EQ -> a
                        GT -> a
                        LT -> b

myMinimumBy f = myMaximumBy g 
    where g a b = case f a b of
                    EQ -> EQ
                    GT -> LT
                    LT -> GT    

     