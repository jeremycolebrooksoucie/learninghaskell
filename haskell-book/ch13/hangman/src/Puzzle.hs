module Puzzle where

import Data.List (intersperse)

data Puzzle = Puzzle String       -- the word
                     [Maybe Char] -- parts of word user can see
                     [Char]       -- guessed chars so far

instance Show Puzzle where
    show (Puzzle _ discovered guessed) = word ++ guessed'
        where word     = intersperse ' ' $ fmap renderChar discovered
              guessed' = "\nGuessed so far: " ++ guessed
              renderChar = maybe '_' id

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s
                       (map (\_ -> Nothing) s)
                       [] 

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

charGuessed :: Puzzle -> Char -> Bool
charGuessed (Puzzle _ _ guessed) c = elem c guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filled guessedChars) c =
    Puzzle word filled' guessedChars'
    where mergeChar wordChar guessedChar =
            if wordChar == c then Just wordChar
                                else guessedChar
          filled' = zipWith mergeChar word filled
          guessedChars' = if c `elem` word then guessedChars 
                                           else c : guessedChars