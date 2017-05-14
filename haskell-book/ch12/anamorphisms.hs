myIterate :: (a -> a) -> a -> [a]
myIterate f a = f a : myIterate f a

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = 
        case f b of 
            Nothing -> []
            Just (a, b') -> a : myUnfoldr f b'

myIterate' :: (a -> a) -> a -> [a]
myIterate' f = myUnfoldr (\b -> Just (f b, b))

