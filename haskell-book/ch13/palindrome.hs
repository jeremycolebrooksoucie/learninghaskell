import Control.Monad    
import System.Exit (exitSuccess)
import Data.Char
palindrome :: IO ()     
palindrome = forever $ do
    line1 <- fmap clean getLine 
    case (line1 == reverse line1) of
        True  -> putStrLn "It's a palindrome!"
        False -> do 
            putStrLn "Nope!"
            exitSuccess

    where clean = map toUpper . filter isLetter