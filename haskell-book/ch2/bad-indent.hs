-- File to place around with when indentation matters
-- foo x = 
-- let y = x * 2
--  z = x ^ 2
--    in 2 * y * z

-- The correct version
foo x = 
    let y = x * 2
        z = x ^ 2
    in 2 * y * z 
