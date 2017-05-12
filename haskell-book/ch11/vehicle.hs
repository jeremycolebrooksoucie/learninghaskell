data Vehicle = Car Manufacturer Price
             | Plane Airline
        deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
    deriving (Eq, Show)


data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
    deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

-- 2
isCar (Car _ _) = True 
isCar _         = False

isPlane (Plane _) = True
isPlane _         = False

areCars = map isCar

getManu (Car m _) = Just m
getManu _         = Nothing
