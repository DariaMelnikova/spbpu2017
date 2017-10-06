import Prelude hiding (foldr, foldl, map, concat, reverse)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ start []       = start
foldr f start (x : xs) = f x (foldr f start xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ start [] = start
foldl f start (x : xs) = foldl f (f start x) xs

map :: (a -> b) -> [a] -> [b]
map f  = foldr (\ x xs -> (f x) : xs) []

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f = foldr (\ x xs -> (f x) ++ xs) [] 

concat :: [a] -> [a] -> [a]
concat = flip $ foldr (:)

maxBy :: (a -> Integer) -> [a] -> a
maxBy f (x : xs) = foldr (\ x prev -> if f x > f prev then x else prev) x xs

minBy :: (a -> Integer) -> [a] -> a
minBy f (x : xs) = foldr (\ x prev -> if f x < f prev then x else prev) x xs

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

elementAt :: Integer -> [a] -> a
elementAt n lst = head $ foldl (const.tail) lst [1..n]

indexOf :: String -> [String] -> Integer
indexOf s list = r 
     where (Just r) = foldr (\ (a, b) result -> if a == s then Just b else result) Nothing (zip list [1..])
