data BinaryTree = EmptyTree
                | Leaf Integer BinaryTree
                | Node Integer BinaryTree BinaryTree BinaryTree deriving Show

insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = Leaf x EmptyTree
insert (Leaf l p) x | x < l = let newNode  = Node l (Leaf x newNode) EmptyTree p in newNode
                    | otherwise = let newNode = Node l EmptyTree (Leaf x newNode) p in newNode
insert (Node v l r p) x | x < v = Node v (insert l x) r p
                        | otherwise = Node v l (insert r x) p
                             
remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree
remove leaf@(Leaf l p) x = if (l == x) then EmptyTree else leaf
remove (Node v l r p) x | x < v = Node v (remove l x) r p
                        | x > v = Node v l (remove r x) p
                        | otherwise = concat l r
                  where concat EmptyTree t = t
                        concat (Leaf v p) t = Node v EmptyTree t p
                        concat (Node v l r p) t = Node v l (concat r t) p

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf l p)  x = x == l
containsElement (Node v l r p) x | x < v = containsElement l x
                                 | x > v = containsElement r x
                                 | otherwise = v == x
