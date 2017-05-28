module MonoidEx where

import Data.Monoid
--
-- Not going to redo all of the semigroup stuff,
--  Just going to do 8 because it looks hard
--

--
-- 8
--

newtype Mem s a = 
    Mem {
        runMem :: s -> (a, s)
    }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \a -> (mempty, a)
    mappend (Mem f) (Mem g) = Mem combine 
        where combine s = 
                let (a,  s') = f s
                    (a', s'') = g s' 
                in (a <> a', s'')