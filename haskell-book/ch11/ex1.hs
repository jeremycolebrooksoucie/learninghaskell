{-# LANGUAGE FlexibleInstances #-}
class TooMany a where
    tooMany :: a -> Bool


newtype ISty = IS (Int, String)

instance TooMany ISty--(Int, String)
    where tooMany (IS (i, s)) = show i == s

-- this requires FlexibleInstances
instance TooMany (Int, String)
    where tooMany (i, s) = show i == s

instance TooMany Int 
    where tooMany = (<40)

--instance TooMany (Int, Int)
--    where tooMany (x, y) = x + y < 42

instance (Num a, TooMany a) => TooMany  (a, a)
    where tooMany (x, y) = tooMany $ x + y