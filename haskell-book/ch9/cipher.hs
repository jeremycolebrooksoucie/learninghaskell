module Cipher where

import Data.Char
caesar :: Int -> String -> String

caesar a text = map (applyAlpha rotate) text
    where rotate = toLet . (`rem` 52) . (+a) . toNum 
          
          applyAlpha f c = if isLetter c then f c else c
          
          toNum c | isLower c = ord c - ord 'a' 
                  | isUpper c = ord c - ord 'A' + 26
                  | otherwise = ord c

          toLet i | i >= 26   = chr $ i + ord 'A' - 26
                  | otherwise = chr $ i + ord 'a'  

unCaesar a = caesar (-a)