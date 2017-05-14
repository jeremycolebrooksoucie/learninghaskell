lefts :: [Either a b] -> [a]
lefts = foldr select []
    where select (Left x)  xs = x : xs
          select (Right x) xs = xs

rights :: [Either a b] -> [b]
rights = foldr select []
    where select (Left x)  xs = xs
          select (Right x) xs = x : xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers lrs = (lefts lrs, rights lrs)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe f (Left  a) = Nothing
eitherMaybe f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)


