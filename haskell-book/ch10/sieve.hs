--import Data.Sequence

--sieve = go 2 empty
--    where go n ps = if any (\p -> p n) ps 
--                        then go (n + 1) ps
--                        else n : go (n + 1) (ps |> (divides n)) 
--          go :: Integer -> Seq (Integer -> Bool) -> [Integer]

divides x y = y `rem` x == 0

sieve :: [Integer]
sieve = go [2 ..]
    where go (p : rest) = p : (go $ filter (not . divides p) rest)

prime1 =go [2 ..] 
    where go (x:xs) = x : go [n | n <- xs, rem n x /= 0] 

prime2 = 2 :  seive prime2 [3 ..]
    where seive (p:ps) (x:xs) = x : seive ps [n | n <- xs, rem n p /= 0]





--primesPT1 = 2 : sieve primesPT1 [3..] 
--    where sieve (p:ps) xs = let (h,t) = span (< p*p) xs 
--                            in h ++ sieve ps [x | x <- t, rem x p > 0]
