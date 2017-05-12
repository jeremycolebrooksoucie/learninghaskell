etfBool :: Bool -> Bool -> [Bool]

etfOrdering :: Ordering -> Ordering -> [Ordering]

etfInt :: Int -> Int -> [Int]

etfChar :: Char -> Char -> [Char]

etf start end | start < end  = start : (etf (succ start) end)
              | start > end  = start : (etf (pred start) end)             
              | start == end = [start]

etfBool = etf
etfOrdering = etf
etfInt = etf
etfChar = etf