import Data.Monoid

data Optional a = 
          None
        | Some a
    deriving (Eq, Show)
instance Monoid a => Monoid (Optional a) where
    mempty = None 
    mappend None None = None
    mappend (Some a) (Some b) = Some $ a <> b
    mappend None     (Some a) = (Some a)
    mappend (Some a) None     = (Some a)
