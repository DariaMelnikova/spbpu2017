nod :: Integer -> Integer -> Integer
nod a b | a == b = a
        | a > b = if a `mod` b == 0 then b else nod b (a - b)
        | otherwise = nod b a

isSquareBetween :: Integer -> Integer -> Bool
isSquareBetween a b = (floor $ sqrt $ toDbl b) >= ( ceiling $ sqrt $ toDbl a )
                    where toDbl x = fromIntegral x::Double

isDataCorrect :: Integer -> Integer -> Integer -> Bool
isDataCorrect day month year | month == 2 = if year `mod` 4 == 0 then day <= 29 else day <= 28
                             | month > 12 || month < 1 = False
                             | otherwise = day <= find cal && day > 0
                            where cal = [(1, 31), (2, 28), (3, 31), (4, 30), (5, 31), (6, 30), (7, 31), (8, 31), (9, 30), (10, 31), (11, 30), (12, 31)]
                                  find [] = 0
                                  find (a:x) = if fst a == month then snd a else find x
                               
