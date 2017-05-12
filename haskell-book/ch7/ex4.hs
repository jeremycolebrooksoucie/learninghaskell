simpleTest x 
    | x == True = t
    | x == False = f 
    where t = "This is true"
          f = "This is false"

avgGrade grades = 
    case sum grades / (fromIntegral $ length grades) of
        x | x > 0.9   -> "A"
          | otherwise -> "Not A" 

numbers x 
    | x < (0 :: Integer) = -1
    | x == 0 = 0
    | x > 0 = 1