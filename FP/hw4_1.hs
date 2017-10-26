data FunMonad a = FunMonad { fun :: () -> a } 

instance (Show a)=> Show (FunMonad a) where
    show (FunMonad a) = "FunMonad " ++ (show $ a ())
-- I need it for usage example, shows only a    

instance Functor FunMonad where
    fmap f (FunMonad x) = FunMonad (\() -> f (x ()) )

instance Applicative FunMonad where
    pure x = FunMonad $ \() -> x
    FunMonad a <*> FunMonad b = FunMonad $ \() -> (a ()) (b ())

instance Monad FunMonad where
    return a  = FunMonad ( \() -> a )
    m >>= k = FunMonad $  \() -> fun ( k (fun m ()) ) ()
    fail = error


fm =  FunMonad $ \()-> 5     -- 'FunMonad 5'
f x = FunMonad $ \() -> 2*x  -- 'FunMonad 2*

test = fm >>= f              -- Should be 'FunMonad 10'
 
