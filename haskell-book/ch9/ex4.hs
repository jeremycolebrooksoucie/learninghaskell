--1
import Data.Set (Set, fromList, member)
isMult3 = filter (\x -> (x `rem` 3) == 0)

numMult3 = length . isMult3

removeArticles :: String -> [String]
removeArticles = (filter $ not . flip member articles) . words
    where articles = fromList ["a", "an", "the"]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _  = []
myZipWith _  _ [] = []
myZipWith f (a : as) (b : bs) = f a b : myZipWith f as bs 

myZip = myZipWith (,)

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip ((a, b) : rest) = (a:as, b:bs)
    where (as, bs) = myUnzip rest
myUnzip [] = ([], [])