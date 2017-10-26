data ReverseList a = RNil | RCons (ReverseList a) a

toList RNil = []
toList (RCons xs x) = (toList xs) ++ [x]

toRList [] = RNil
toRList x = RCons (toRList $ init x) (last x)


instance (Show a) => Show (ReverseList a) where
    show l = show (toList l)

instance (Eq a) => Eq (ReverseList a) where
    (==) a b = (==) (toList a) (toList b)

instance (Ord a) => Ord (ReverseList a) where
    (<=) a b = (<=) (toList a) (toList b)

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend RNil y = y
    mappend x RNil = x
    mappend x (RCons y z) = RCons (mappend x y) z

instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons xs x) = RCons ( fmap f xs ) (f  x)
