module Arith where

imult   :: Integer -> Integer -> Integer
isum    :: Integer -> Integer -> Integer
inegate :: Integer -> Integer
idiv    :: Integer -> Integer -> Integer



isum = (+)
inegate = negate
imult 0 _ = 0
imult x y = isum y $ imult (x - 1) y 

idiv x y = 