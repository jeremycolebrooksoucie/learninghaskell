
import Data.Char

vigenere :: String -> String -> String
vigenere key = unwords . map cypher . words . clean 
    where clean   = filter isAlpha . map toUpper
          key_rep = concat . repeat . clean $ key
          cypher w = map rotate $ zip key_rep w
          rotate (x, c) = unNum . (`mod` 26) . (+ offset) . toNum $ x 
              where offset = (ord c - ord 'A')
                    toNum  = (+ (- ord 'A')) . ord
                    unNum  = chr . (+ ord 'A')

unvigenere :: String -> String -> String
unvigenere key = vigenere $ map (unNum . (`mod` 26) . (+26) .  negate . toNum) . clean $ key
    where clean   = filter isAlpha . map toUpper
          toNum  = (+ (- ord 'A')) . ord
          unNum  = chr . (+ ord 'A')
