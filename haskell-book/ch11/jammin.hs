module Jammin where

import Data.List

data Fruit = 
     Peach
   | Plum
   | Apple
   | Blackberry
   deriving (Eq, Show, Ord)

data JamJars = Jam { kind :: Fruit, 
                     quantity :: Int }  
    deriving (Eq, Show, Ord)

-- Cardinality of JamJars is 4 * (maxBound :: Int - MinBound :: Int + 1)

row1 = Jam {kind = Peach, quantity = 5}
row2 = Jam {kind = Plum, quantity = 2}
row3 = Jam {kind = Blackberry, quantity = 50}
row4 = Jam {kind = Apple, quantity = 52}
row5 = Jam {kind = Peach, quantity = 1}
allJam = [row1, row2, row3, row4, row5]

types = map kind allJam
quantities = map quantity allJam

numJars = sum . map quantity

compareKind j j' = compare (kind j) (kind j')

sortByKind = sortBy compareKind

groupByKind = groupBy (curry $ (== EQ) . uncurry compareKind) . sortByKind