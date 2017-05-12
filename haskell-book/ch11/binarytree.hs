data Tree a = Leaf 
            | Node (Tree a) a (Tree a)
    deriving Show


insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left x' right) = 
            case compare x x' of
                EQ -> Node left x right
                GT -> Node (insert x left) x' right
                LT -> Node left x' (insert x right)


--treeMap _ Leaf = Leaf
--treeMap f (Node left x right) = Node (treeMap f left) 
--                                          (f x)
--                                          (treeMap f right)
treeMap f = treeFoldr (\a b -> insert (f a) b) Leaf


treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr f n Leaf = n
treeFoldr f n (Node left x right) = treeFoldr f (f x rval) left
    where rval = treeFoldr f n right

treeToList :: Tree a -> [a]
treeToList = treeFoldr (:) []


inOrder Leaf = []
inOrder (Node left x right) = l ++ [x] ++ r
    where l = inOrder left
          r = inOrder right