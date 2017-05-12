module Database where

import Data.Time

--
-- Do all of these with folds
--

data DatabaseItem = DbString String 
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]

theDatabase = 
    [ DbDate $ UTCTime (fromGregorian 1911 5 1) 
                       (secondsToDiffTime 34123),
      DbNumber 9001,
      DbString "Hello World",
      DbDate $ UTCTime (fromGregorian 1921 5 1) 
                       (secondsToDiffTime 34123)]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where f (DbDate t) rest = t:rest
          f _ rest = rest 
          f :: DatabaseItem -> [UTCTime] -> [UTCTime]

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where f (DbNumber t) rest = t:rest
          f _ rest = rest 
          f :: DatabaseItem -> [Integer] -> [Integer]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb db = (/ l) . fromIntegral . sum $ ns
    where ns = filterDbNumber db
          l  = fromIntegral . length $ ns
