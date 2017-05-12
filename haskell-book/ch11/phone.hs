

import Data.List
import Data.Char
import Control.Monad

type Phone = [(Digit, [Char])]

myPhone =
    [('1', "1"),     ('2', "abc2"), ('3', "def3"),
     ('4', "ghi4"),  ('5', "jkl5"), ('6', "mno6"), 
     ('7', "pqrs7"), ('8', "tuv8"), ('9', "wxyz9"),
     ('*', "^*"),    ('0', "+ 0"),  ('#', ".,#")]
upperPress = ('^', 1) :: (Digit, Press)

type Digit = Char 
type Press = Int


textToTaps :: Phone -> String -> [(Digit, Press)]

textToTaps phone t = concat . map (maybe [] id) . map (charToTaps phone) $ t 

charToTaps :: Phone -> Char -> Maybe [(Digit, Press)]
charToTaps phone c 
    | isUpper c = fmap (upperPress :) $ charToTaps phone $ toLower c
    | otherwise = fmap (\x -> [x]) digitpresses
    where digitdata         = find (any (== c) . snd ) phone
          digitpresses      = join $ fmap lookupChar digitdata
          lookupChar (d, t) = fmap (\i -> (d, i + 1)) $ findIndex (== c) t

-- Crappy old version without fmap
--charToTaps phone c 
--    | isUpper c = case (charToTaps phone $ toLower c)
--                    of Just x -> Just (upperPress : x)
--                       Nothing -> Nothing
--    | otherwise = case (digit, presses)
--                    of (Just d, Just p) -> Just [(d, p)]
--                       _                -> Nothing  
--    where (digit, text) = 
--        case findIndex (any (== c) . snd ) phone
--                        of Nothing -> (Nothing, Nothing)
--                           Just i  -> (Just $ fst $ phone !! i, 
--                                       Just $ snd $ phone !! i)
--          presses   = case text 
--                        of Nothing -> Nothing
--                           Just t  -> case findIndex (== c) t 
--                                        of Nothing -> Nothing
--                                           Just i  -> Just (i + 1)  



fingerTaps :: [(Digit, Press)] -> Press
fingerTaps = sum . map snd

mostPopular :: String -> Char
mostPopular = fst . maximumBy pickBest . map toSizePair . groupBy (==) . sort 
    where toSizePair xs = (head xs, length xs) 
          pickBest (x, l) (x', l') = compare l l'
