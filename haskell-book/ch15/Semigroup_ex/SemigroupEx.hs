module SemigroupEx where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck
import Control.Monad

type IntSum = Sum Int

-- 
-- 1
--

data Trivial = Trivial deriving (Show, Eq)

instance Semigroup Trivial where
    Trivial .++. Trivial = Trivial
instance Arbitrary Trivial where
    arbitrary = return Trivial
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


-- 
-- 2
--

data Identity a = Identity a deriving (Show, Eq)

instance Semigroup (Identity a) where
    Identity a .++. Identity b = Identity a
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary =fmap Identity arbitrary 
type StringIdentityAssoc =  Identity String -> Identity String -> Identity String -> Bool


-- 
-- 3
--

data Two a b = Two a b deriving (Show, Eq)

instance Semigroup (Two a b) where
    Two a b .++. Two a' b' = Two a b
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =  do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type StringSumTwoAssoc =  Two String IntSum -> Two String IntSum  -> Two String IntSum -> Bool

--
-- 4 and 5 are generalizations of two
--


-- 
-- 6
--

data BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    BoolConj True .++. BoolConj True = BoolConj True
    BoolConj True .++. BoolConj False = BoolConj False
    BoolConj False .++. BoolConj True = BoolConj False
    _ .++. _ = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = fmap BoolConj arbitrary

type BoolConjAssoc =  BoolConj -> BoolConj  -> BoolConj -> Bool

--
-- 7 is like 6
--


--
-- 8
--


data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
    Fst a .++. Fst b = Fst b
    Snd a .++. Snd b = Snd a
    Fst a .++. Snd b = Snd b
    Snd a .++. Fst b = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary =  do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]

type StringSumOrAssoc =  Or String IntSum -> Or String IntSum  -> Or String IntSum -> Bool



--
-- 9
--

newtype Combine a b = 
    Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    Combine f .++. Combine g = Combine (\a -> f a .++. g a)



--
-- 10 is like 9
--

--
-- 11
--


data Validation a b = Fail a | Succ b deriving (Show, Eq)

instance Semigroup (Validation a b) where
    Fail a .++. Fail b = Fail a
    Succ a .++. Succ b = Succ a
    Fail a .++. Succ b = Succ b
    Succ a .++. Fail b = Succ a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary =  do
        a <- arbitrary
        b <- arbitrary
        elements [Fail a, Succ b]

type StringSumValidationAssoc =  Validation String IntSum -> Validation String IntSum  -> Validation String IntSum -> Bool


--
-- 12
-- 


newtype AccumulateRight a b = AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup (AccumulateRight a b) where
    l .++. r = r
    

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary =  do
        a <- arbitrary
        b <- arbitrary
        elements [AccumulateRight $ Fail a, AccumulateRight $ Succ b]

type StringSumAccumRightAssoc =  Validation String IntSum -> Validation String IntSum  -> Validation String IntSum -> Bool

--
-- 13 is like 12
-- 