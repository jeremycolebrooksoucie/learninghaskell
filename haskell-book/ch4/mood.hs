module Mood where

data Mood = Woot | Blah deriving Show

changeMood Woot = Blah
changeMood _    = Woot
