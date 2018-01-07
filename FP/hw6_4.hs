data BinaryTree = EmptyTree
                | Leaf Integer BinaryTree
                | Node { value::Integer,
                         left:: BinaryTree,
                         right::BinaryTree,
                         parent::BinaryTree
                       } deriving Show

insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = Leaf x EmptyTree
insert node@(Leaf l p) x | x < l = let newNode  = Node l (Leaf x newNode) EmptyTree ( update_child p newNode) in newNode
                         | otherwise = let newNode = Node l EmptyTree (Leaf x newNode) (update_child p newNode) in newNode
                              where update_child (Node v0 node r0 p0) child = Node v0 child r0 p0
                                    update_child (Node v0 l0 node p0) child = Node v0 l0 child p0
                                    update_child _ _ = error "wrong parent node"
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
                        concat p (Leaf v p0) t = let node = Node v EmptyTree (concat node EmptyTree t) (update_child p node) in node
                        concat p (Node v l r p0) t = let node = Node v l (concat node r t) (update_child p node) in node
                        update_child (Node v0 node r0 p0) child = Node v0 child r0 p0
                        update_child (Node v0 l0 node p0) child = Node v0 l0 child p0
                        update_child _ _ = error "wrong parent node"

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf l p)  x = x == l
containsElement (Node v l r p) x | x < v = containsElement l x
                                 | x > v = containsElement r x
                                 | otherwise = v == x
