-------------------------------
-- Multiple Choice           --
-------------------------------
-- 1 A polymorphic function may resolve to different
--      types dependingo n inputs

-- 2 f :: Char -> String, g :: String -> [String]
--   g . f :: Char -> [String]

-- 3 d

-- 4 This is poorly written :(

-- 5 Its a bool

-------------------------------
-- Writing Code              --
-------------------------------

-- 1
tensDigit  :: Integral a => a -> a
tensDigit x = m 
    where (d, m) = (x `div` 10) `divMod` 10

hunsD = (flip mod) 10 . (flip div) 100

-- 2
foldBool :: a -> a -> Bool -> a 
foldBool' :: a -> a -> Bool -> a

foldBool x y b = 
    case b of
        True -> x
        False -> y

foldBool' x y b 
    | b == True = x
    | b == False = y


-- 3 
sillyFunc :: (a -> b) -> (a, c) -> (b, c)
sillyFunc f (a, c) = (f a, c)

