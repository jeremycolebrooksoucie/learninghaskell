module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import Puzzle
import System.Random (randomRIO)

type WordList = [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
    



allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict) 

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w = 
            let l = length w
            in  l > minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, l) --replace this with randomness later
    return $ wl !! randomIndex
    where l = length wl

randomWord' :: IO String
randomWord' = gameWords >>= randomWord    


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord  puzzle guess,
          charGuessed puzzle guess) of
        (_, True) -> do
            putStrLn $ "You already guessed that character, "  
                       ++ "guess again"
            return puzzle
        (True, _) -> do
            putStrLn $ "This character was in the word, "
                       ++ "filling in the word accordingly"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn $ "This character wasn't in the word. "
                       ++ "Try again."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) = 
    if (length guessed) > 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ word
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filled _) = 
    if all isJust filled then
        do putStrLn "You win!"
           exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin  puzzle
    putStrLn $ "Current puzzle is: \n" ++ show puzzle
    putStr "Guess a character"
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "You guess must be a single char" 

