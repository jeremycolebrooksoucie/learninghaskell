-- Part 1
-- 1
-- a: yes
-- b: no
-- c: yes
-- d: no, missing closing quote
-- e: no
-- f: yes
-- g: no
-- h: yes

-- 2
-- a -> d
-- b -> c
-- c -> e
-- d -> a
-- e -> b
--
--

-- Part 2
-- 2
part_a = (++ "!")
part_b = drop 4 . take 5
part_c = drop 9

-- Part 3
thirdLetter :: String -> Char
thirdLetter = (!! 2)
