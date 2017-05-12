data OperatingSystem = 
       Gnu
     | OpenBSD
     | Mac
     | Windows
     deriving (Eq, Show, Ord, Bounded, Enum)

data ProgrammingLanguage = 
       Haskell
     | Agda
     | Idris
     | Purescript
     deriving (Eq, Show, Ord, Bounded, Enum)

data Programmer = 
    Programmer { os   :: OperatingSystem,
                 lang :: ProgrammingLanguage}
    deriving (Eq, Show, Ord, Bounded)

enumerateProgrammers = [(pl, os) | pl <- enumFrom (minBound :: ProgrammingLanguage),
                                   os <- enumFrom (minBound :: OperatingSystem)]