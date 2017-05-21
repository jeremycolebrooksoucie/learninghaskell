import Text.Read (readMaybe)

type Name = String
type Age  = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person 

mkPerson name age 
    | name /= "" && age > 0 = Right $ Person name age
    | name =="" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ 
                           "Name was: " ++ show name ++
                           " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson =  do
    ePerson <- fmap parsePrompt prompt

    
    case ePerson of
        Right person   -> putStrLn $ "Yay : " ++ show person
        Left NameEmpty -> putStrLn $ "Error: name field was empty" 
        Left AgeTooLow -> putStrLn $ "Error: provided age less than 0"
        Left (PersonInvalidUnknown t) -> putStrLn $ t



    where parsePrompt :: (Maybe Name, Maybe Age) -> Either PersonInvalid Person
          parsePrompt (Just name, Just age) = mkPerson name age 
          parsePrompt _  = Left $ PersonInvalidUnknown $ "Parse error occured on input"

          prompt :: IO (Maybe Name, Maybe Age)
          prompt = do
                putStrLn "Enter name"
                mName <- fmap Just getLine
                putStrLn "Enter age"
                mAge  <- fmap readMaybe getLine
                return (mName, mAge)