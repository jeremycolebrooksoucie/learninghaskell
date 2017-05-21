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

uncaesar a = caesar (-a)


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


main :: IO ()
main = do
    putStrLn "Enter text to encrypt"
    text <- getLine
    putStrLn "Enter key"
    key  <- getLine

    prompt <- getPromptType
    case prompt of 
        Caesar     -> putStrLn $ caesar (read key) text
        Uncaesar   -> putStrLn $ uncaesar (read key) text  
        Vigenere   -> putStrLn $ vigenere key text
        Unvigenere -> putStrLn $ unvigenere key text

    


data CipherType  = Caesar | Vigenere | Unvigenere | Uncaesar
        deriving (Show, Eq)

getPromptType :: IO (CipherType)
getPromptType = do
    putStrLn "Enter cipher type: vigenere [1] caesar [2]\
                                \ unvigenere [3] or unCaesar [4]"
    text <- getLine
    case text of 
      "1" -> return Vigenere
      "2" -> return Caesar
      "3" -> return Unvigenere
      "4" -> return Uncaesar
      otherwise -> do
            putStrLn "Please enter either 1, 2, 3, or 4 "
            getPromptType  

