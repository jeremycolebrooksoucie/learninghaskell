partitionEithers :: [(Either a b)] -> ([a], [b])

partitionEithers []  = ([], [])
partitionEithers (e : rest) = case e of
        Right b -> (as    , b : bs)
        Left  a -> (a : as, bs)
    where (as, bs) = partitionEithers rest
