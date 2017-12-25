data Dyn = Fun (Dyn -> Dyn)
         | App { 
                 function :: Dyn,
                 argument :: Dyn
               }
         | Char Char
         | Int Integer


instance Show Dyn where
    show (Char a)   = "Dynamic " ++ show a
    show (Int a)    = "Dynamic " ++ show a
    show (Fun f)    = "some function"
    show (App f a)  = "apply some function to " ++ show a


instance Eq Dyn where
    (==) (Char a)  (Char b)       = a == b
    (==) (Int a)   (Int b)        = a == b
    (==) (App (Fun f) a) b        = (f a) == b
    (==) b (App (Fun f) a)        = (f a) == b
    (==) _ _                      = error "Not supported"

instance Ord Dyn where
    (<=) (Char a)  (Char b) = a <= b
    (<=) (Int a)   (Int b)  = a <= b
    (<=) (App (Fun f) a) b  = (f a) <= b
    (<=) b (App (Fun f) a)  = b <= (f a)
    (<=) _ _                = error "Not supported"

   
instance Num Dyn where
    (+) (Int a)   (Int b)  = Int $ a + b
    (+) (App (Fun f) a) b  = (f a) + b
    (+) b (App (Fun f) a)  = (f a) + b
    (+) _ _                = error "Not supported"
    
    (*) (Int a)   (Int b)  = Int $ a * b
    (*) (App (Fun f) a) b  = (f a) * b
    (*) b (App (Fun f) a)  = (f a) * b
    (*) _ _                = error "Not supported"    

    negate (Int a)         = Int $ negate a
    negate (App (Fun f) a) = negate $ f a
    negate _               = error "Not supported"

    signum (Int a)         = Int $ signum a
    signum (App (Fun f) a) = signum (f a)
    signum _               = error "Not supported"
    
    abs (Int a)         = Int $ abs a
    abs (App (Fun f) a) = abs (f a)
    abs _               = error "Not supported"
    
    fromInteger = Int

instance Enum Dyn where
    toEnum =  Int . toInteger 

    fromEnum (Char c)        = fromEnum c
    fromEnum (Int i)         = fromEnum i
    fromEnum (App (Fun f) a) = fromEnum $ f a
    fromEnum _               = error "Not supported" 

instance Real Dyn where
    toRational (Int i)         = toRational i
    toRational (App (Fun f) a) = toRational $ f a
    toRational _               = error "Not supported"

instance Integral Dyn where
    toInteger (Int i)         = i
    toInteger (App (Fun f) a) = toInteger $ f a
    toInteger _               = error "Not supported"

    quotRem (Int a) (Int b)     = let (c, d) = quotRem a b in (Int c, Int d)
    quotRem (App (Fun f) a)  b  = quotRem (f a) b
    quotRem a   (App (Fun f) b) = quotRem a (f b)
    quotRem _ _                 = error "Not supported"      




i :: Dyn -> Dyn
i = id

k :: Dyn -> Dyn -> Dyn
k  = App

s :: (Dyn -> Dyn -> Dyn) -> (Dyn -> Dyn) -> Dyn -> Dyn
s f1 f2 a = App (Fun (\a2 -> f1 a a2)) (App (Fun f2) a)
