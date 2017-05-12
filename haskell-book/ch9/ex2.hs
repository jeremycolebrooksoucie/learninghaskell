mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tuples = [(m, n) | m <- mySqr, n <- myCube]
tuplesFiltered = [(m, n) | m <- mySqr, n <- myCube, m < 50, n < 50]
lenTuples = length tuplesFiltered