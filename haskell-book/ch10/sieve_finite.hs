sieve n = sortedRemoveMultiples [2 .. n^2] [2 .. n]
isFactor m n = (m /= n) && (n `rem` m == 0)

-- removes all ys from xs
sortedRemove :: [Integer] -> [Integer] -> [Integer]
sortedRemove (x:xs) (y:ys) 
                | x == y = sortedRemove xs ys
                | y >  x = x : sortedRemove xs (y:ys)
                | x >  y = x : sortedRemove xs ys
sortedRemove [] [] = []
sortedRemove xs [] = xs
sortedRemove [] _  = []

sortedRemoveMultiples xs (r : rest) = sortedRemoveMultiples cleaned rest
    where cleaned = sortedRemove xs $ multipleOf r
sortedRemoveMultiples xs [] = xs

multipleOf r = [r * (i + 1) | i <- [1 ..]]