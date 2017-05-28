module Main where

import Test.QuickCheck
import Data.Semigroup
import Data.Monoid

import SemigroupEx
import MonoidEx

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = ((a .++. b) .++. c) == (a .++. (b .++. c))


main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: StringIdentityAssoc)
    quickCheck (semigroupAssoc :: StringSumTwoAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: StringSumOrAssoc)
    quickCheck (semigroupAssoc :: StringSumValidationAssoc)
    quickCheck (semigroupAssoc :: StringSumAccumRightAssoc)
    testMem

testMem :: IO ()
testMem = do
    print $ runMem (f' <> mempty) 0    
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0
    where f' = Mem $ \s -> ("hi", s + 1)



