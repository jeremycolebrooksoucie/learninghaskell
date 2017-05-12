import Data.List

-----------------------------
-- Multiple Choice         --
-----------------------------
-- 1 : c
-- 2 : b
-- 3 : a
-- 4 : c
-- 5 : a

-----------------------------
-- Does it typecheck       --
-----------------------------
-- 1
data Person = Person Bool deriving Show
 -- Needed to add deriving show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah
          | Woot deriving (Show, Eq)
-- Needed to add Eq to derived type classes
settleDown x = if x == Woot then Blah else x

-- 3 
-- Settle down will accept all Moods. It will thus 
--  fail on 9.
-- Mood does implement Ord, so Blah > Woot will fail

-- 4
type Subject = String
type Verb    = String
type Object  = String

data Sentence = 
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
-- This type checks.
-- Note that s1 is an incomplete type so it won't 
--  print

-----------------------------
-- Datatypes               --
-----------------------------
data Rocks = 
    Rocks String deriving (Eq, Show)

data Yeah = 
    Yeah Bool deriving (Eq, Show)

data Papu = 
    Papu Rocks Yeah
    deriving (Eq, Show)

-- Which of the following typecheck and which don't

--1 
-- Original phew = Papu "Chases" True
-- Corrected
phew = Papu (Rocks "Chases")  (Yeah True)

-- 2
truth = Papu (Rocks "chomskydoz") (Yeah True)
-- this is fine

-- 3
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p
-- this is fine

-- 4 
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
 -- These break beacuse Papu doesn't implement Ord


-----------------------------
-- Some type inference     --
-----------------------------

-- 1 This one breaks
-- i :: a
-- i = 1 

-- 2  This doesn't work because 1.0 is more 
--      specific than Num
--f :: Num a => a
--f = 1.0

-- 3 This is fine because Fractional is as
--      speicifc as 1.0
f' :: Fractional a => a
f' = 1.0

-- 4 Works because RealFrac is subtype of Fractional
f'' :: RealFrac a => a
f'' = 1.0

-- 5 Fine
freud :: Ord a => a -> a
freud x = x

-- 6 Fine
freud' :: Int -> Int
freud' x = x

-- 7
--  This one break
--myX = 1 :: Int
--sigmund :: a -> a
--sigmund x = myX

-- 8
-- This one also break
--myX = 1 :: Int
--sigmund' :: Num a => a -> a
--sigmund' x = myX


-- 9
-- This is fine because concrete Int type has Ord
jung :: [Int] -> Int
jung xs = head (sort xs)


-- 10
-- This is fine
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11 
mySort :: [Char] -> [Char]
mySort = sort

-- 12 This is also fine since char has ord
signifier :: [Char] -> Char
signifier xs = head (mySort xs)


-----------------------------
-- Type Kwon Do            --
-----------------------------
-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

--2 
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a