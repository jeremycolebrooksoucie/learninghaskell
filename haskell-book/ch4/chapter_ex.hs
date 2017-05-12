-- chapter excercises

awesome     = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome  = [awesome, alsoAwesome]

-- 1: length: [a] -> b

-- 2: a => 5, b => 3, c => 2, d => 5

-- 3 - 7: trivial

-- 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome word = word == reverse word

-- 9 
myAbs :: Integer -> Integer
myAbs i = if i < 0 then (-i) else i

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))


