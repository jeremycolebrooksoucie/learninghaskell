class Numberish a where
    fromNumber :: Integer -> a
    toNumber   :: a -> Integer

newtype Age = 
    A Integer
    deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = A n
    toNumber (A n) = n

newtype Year =
    Y Integer
    deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Y n
    toNumber (Y n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed 
    where integer1 = toNumber a
          integer2 = toNumber a'
          summed = integer1 + integer2
