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
                        | otherwise = concat p l r
                  where concat p EmptyTree EmptyTree = EmptyTree
                        concat p EmptyTree (Leaf v p0) = Leaf v p
                        concat p EmptyTree (Node v l r p0) = Node v l r p
                        concat p (Leaf v p0) t = let node = Node v EmptyTree (concat node EmptyTree t) p in node
                        concat p (Node v l r p0) t = let node = Node v l (concat node r t) p in node

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf l p)  x = x == l
containsElement (Node v l r p) x | x < v = containsElement l x
                                 | x > v = containsElement r x
                                 | otherwise = v == x
