import Data.Maybe
--------------------------
-- Review of Currying   --
--------------------------

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedConny  :: String -> String
frappe :: String -> String

appedConny = cattyConny "woops"
frappe = flippy "haha"

--------------------------
-- Recursion            --
--------------------------
--2 
sumTo :: (Eq a, Num a) => a -> a
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

--3
myMult :: (Integral a) => a -> a -> a
myMult 0 y = 0
myMult x y  = y + myMult (x - 1) y

-- 3 
myDiv :: (Integral a) => a -> a -> Maybe (a, a)
myDiv _   0   = Nothing
myDiv num den = Just $ f $ go (abs num) (abs den) 0 
    where f (a, b) = (sign a, sign b)
          sign = if num * den > 0 then id else negate
          go num den n 
            | num < den = (n, num)
            | otherwise = go (num - den) den (n + 1)

-- McCarthy 91
mc n 
    | n > 100 = n - 10
    | otherwise = mc (mc (n + 11))