isJust :: Maybe a -> Bool
isJust = mayybee False (\_ -> True)


isNothing :: Maybe a -> Bool
isNothing = mayybee True (\_ -> False)


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a


fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id 


listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList  = maybe [] (\x -> [x])


catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just a) -> a) . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe (Just x:xs) = 
    case flipMaybe xs
      of Just xs'  -> Just $ x : xs'
         otherwise -> Nothing
flipMaybe [] = Just []
flipMaybe (Nothing : _) = Nothing

