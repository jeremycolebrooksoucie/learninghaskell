module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
        -- it "2 + 2 is equal to 4" $ do
        --     (2 + 2) == 4 `shouldBe` True
        -- it "1 + 1 is greater than 1" $ do
        --     (1 + 1) > 1 `shouldBe` True
        -- it "multiply 3 4 should equal 12" $ do
        --     multiply 3 4 == 12 `shouldBe` True
        -- it "multiply -3 4 should equal -12" $ do
        --     multiply (-3) 4 == (-12) `shouldBe` True
        -- it "multiply 3 -4 should equal -12" $ do
        --     multiply 3 (-4) == (-12) `shouldBe` True
        -- it "multiply (-3) (-4) should equal 12" $ do
        --     multiply (-3) (-4) == 12 `shouldBe` True
        -- it "multiply 0 4 should equal 0" $ do
        --     multiply 0 4 == 0 `shouldBe` True
        -- it "multiply 3 0 should equal 0" $ do
        --     multiply 3 0 == 0 `shouldBe` True
        -- it "multiply 1 4 should equal 4" $ do
        --     multiply 1 4 == 4 `shouldBe` True
        -- it "multiply 3 1 should equal 3" $ do
        --     multiply 3 1 == 3 `shouldBe` True

        


-- multiply :: Num a => a -> a -> a
multiply a b | b < 0  = negate $ multiply a (-b)
             | b == 1 = a
             | b == 0 = 0
             | otherwise = a + multiply a (b -1) 
