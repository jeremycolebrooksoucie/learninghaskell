eFrom       :: (Enum a, Ord a) => a -> [a]
eFromThen   :: (Enum a, Ord a) => a -> a -> [a]
eFromTo     :: (Enum a, Ord a) => a -> a -> [a]
eFromThenTo :: (Enum a, Ord a) => a -> a -> a -> [a]

eFrom a = a : (eFrom . succ $ a)

eFromThen start next = eStep start
    where eStep a = a : (eStep . step $ a)
          step | start < next  = findStep start next succ
               | start > next  = findStep start next succ
               | start == next = id
          findStep current target f 
               | current == target = id 
               | otherwise        = f . (findStep (f current) target f)  

eFromTo     = undefined
eFromThenTo = undefined

