import Data.Set (member, fromList)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe word  = Just word

replaceThe :: String -> String
replaceThe = unwords . map (maybe "a" id . notThe) . words

countTheBeforeVowel :: String -> Int
countTheBeforeVowel = length . filter (== "the") . 
                      takeWhile (not .  startsWithVowel) . words
    where startsWithVowel [] = False
          startsWithVowel (x : xs) 
                | isVowel x = True
                | otherwise = False

isVowel x = member x vowels
    where vowels = fromList ['a', 'e', 'i', 'o', 'u']

countVowels :: String -> Int
countVowels = length . filter isVowel

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord string 
        | num_c >= num_v = Just $ Word' string
        | otherwise      = Nothing
    where num_v = countVowels string 
          num_c = length string - num_v    


data Nat = Zero
         | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat 
integerToNat x 
        | x < 0 = Nothing
        | otherwise = Just $ posToNat x
    where posToNat 0 = Zero
          posToNat x = Succ $ posToNat $ x - 1

