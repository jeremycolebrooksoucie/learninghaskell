module WordNumber where


import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord n | n < 0 = "negative-" ++ digitToWord (-n)
              | otherwise = concat . intersperse "-" . map digitToWord $ digits n 
--leadingDigit n | n > (-10) and n < (10) = n
--               | otherwise = leadingDigit (n / 10) 

--restDigits n = (n `quot` 10) `rem` 10

digits :: Int -> [Int]
digits n | n < 0     = let (h : t) = digits (negate n) in (negate h : t)
         | otherwise = go n $  []
        where go n l  | n <  10   = n : l
                      | otherwise = go (n `quot` 10) ((n `rem` 10) : l) 
--digits n | n < (-9)     = leadingDigit n : (digits . negate . restDigits $ n)
--         | n < 10    = [n]
--         | otherwise = leadingDigit n : (digits . restDigits $ n)  

--wordNumber :: Int -> String
--wordNumber = und