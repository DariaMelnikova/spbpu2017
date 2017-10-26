newtype PSet a = PSet { contains :: (a -> Bool) }
newtype PSet' a = PSet' {contains' :: (a -> Bool) }
instance Monoid (PSet a) where
    mempty = PSet (\x -> False)
    mappend (PSet x) (PSet y) = PSet (\z -> (||) (x z) (y z))
-- mappend shows whether at least one of sets contains the element

instance Monoid (PSet' a) where
    mempty = PSet' (\x -> False)
    mappend (PSet' x) (PSet' y) = PSet' (\z -> (&&) (x z) (y z))
-- mapend shows whether both of sets contain the element

-- Actually, I can do more Monoid instances, but it will be unlogical

instance Functor PSet where
    fmap _ _ = PSet (\_ -> False)
-- Looks like this is the only possible realisation.
-- For example:
--    contains :: PSet a -> a -> Bool
--    f :: a -> b
--    
--    I can execute function f on element of type a, with the result of type b.
--    But I can't use type b in 'contains'. 
--    So, I can realise fmap only for a -> a functions. But it's not enough for Functor instance
