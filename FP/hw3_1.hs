data WeirdPeanoNumber = Zero 
                      | Succ (WeirdPeanoNumber) 
                      | Pred (WeirdPeanoNumber)

numToInt :: WeirdPeanoNumber -> Integer
numToInt Zero = 0
numToInt (Succ a) = 1 + (numToInt a)
numToInt (Pred a) = (numToInt a) - 1

intToNum :: Integer -> WeirdPeanoNumber
intToNum x | x > 0 = Succ $ intToNum $ x - 1
           | x > 0 = Pred $ intToNum $ x + 1
           | otherwise = Zero

instance Eq WeirdPeanoNumber where
    (==) a b = ( numToInt a) == (numToInt b)

instance Ord WeirdPeanoNumber where
    (<=) a b = (numToInt a) <= (numToInt b)

instance Show WeirdPeanoNumber where
    show a = show $ numToInt a

instance Num WeirdPeanoNumber where
    a + b = intToNum $ (numToInt a) + (numToInt b) 
    a * b = intToNum $ (numToInt a) * (numToInt b)
    abs = intToNum.abs.numToInt
    signum = intToNum.signum.numToInt
    negate = intToNum.negate.numToInt
    fromInteger = intToNum

instance Enum WeirdPeanoNumber where
    toEnum = fromIntegral
    fromEnum = fromInteger.numToInt

instance Real WeirdPeanoNumber where
    toRational x = toRational $ numToInt x

instance Integral WeirdPeanoNumber where
    toInteger = numToInt
    quotRem x y = (intToNum a, intToNum b) 
             where (a, b) = quotRem (numToInt x) (numToInt y)
            


test = show $ (intToNum 5) + fst ( quotRem (intToNum 23) (intToNum 4) )


