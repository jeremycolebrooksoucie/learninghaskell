module Reverse where

rvrs :: String -> String
rvrs = unwords . map reverse . words

text = "I like some curry please"

main :: IO ()
main = print $ rvrs text
