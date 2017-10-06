data BinaryTree = EmptyTree
                | Leaf Integer
                | Node Integer BinaryTree BinaryTree deriving Show

insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = Leaf x
insert (Leaf l) x | x < l    = Node l (Leaf x) EmptyTree
                  | x > l = Node l EmptyTree (Leaf x)
                  | otherwise = Leaf l                   
-- looks like the equal elements have no meaning for the binary search tree 
insert (Node v l r) x | x < v = Node v (insert l x) r
                      | x > v = Node v l (insert r x)
                      | otherwise = Node v l r

remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree
remove (Leaf l) x = if (l == x) then EmptyTree else Leaf l
remove (Node v l r) x | x < v = Node v (remove l x) r
                      | x > v = Node v l (remove r x)
                      | otherwise = concat l r
                  where concat EmptyTree t = t
                        concat (Leaf l) t = Node l EmptyTree t
                        concat (Node v l r) t = Node v l (concat r t)

emptyTree :: BinaryTree
emptyTree = EmptyTree

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf l)  x = x == l
containsElement (Node v l r) x | x < v = containsElement l x
                               | x > v = containsElement r x
                               | otherwise = v == x

nearestGE :: BinaryTree -> Integer -> Integer
-- Why I can not use Maybe monad here? What have I do with the empty tree?! What have I do when all elements are less then given? 
nearestGE t x  = case nGE t x of
                   Nothing  -> 0
                   (Just r) -> r

nGE :: BinaryTree -> Integer -> Maybe Integer
nGE EmptyTree _ = Nothing
nGE (Leaf a) x = if (x < a) then Just a else Nothing
nGE (Node v l r) x | v == x = Just v
                   | v < x  = nGE r x
                   | v > x  = case nGE l x of
                                Nothing           -> Just v   
                                r @ (Just result) -> if result < v then r else Just v


treeFromList :: [Integer] -> BinaryTree
treeFromList = foldl insert EmptyTree

listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []
listFromTree (Leaf l) = [l]
listFromTree (Node v l r) = (listFromTree l) ++ [v] ++ (listFromTree r)
