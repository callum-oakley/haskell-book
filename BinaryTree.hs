{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

data Tree a = Nil | Tree (Tree a) a (Tree a)
  deriving (Eq, Ord, Show, Functor, Foldable Traversable)

-- instance Foldable Tree
--   where
--     foldMap f (Tree l x r) = mconcat [foldMap f l, f x, foldMap f r]
--     foldMap _ _ = mempty
--     OR
--     foldr f x (Tree l y right) = foldr f (f y (foldr f x right)) l
--     foldr _ x _ = x

insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = Tree Nil x Nil
insert x (Tree left y right)
  | x == y = Tree left y right
  | x < y = Tree (insert x left) y right
  | x > y = Tree left y (insert x right)

preorder :: Tree a -> [a]
preorder Nil = []
preorder (Tree left x right) = x:(preorder left ++ preorder right)

inorder :: Tree a -> [a]
inorder Nil = []
inorder (Tree left x right) = preorder left ++ (x : preorder right)

postorder :: Tree a -> [a]
postorder Nil = []
postorder (Tree left x right) = preorder left ++ preorder right ++ [x]

testTree = foldl (\acc n -> insert n acc) Nil [2, 1, 3]
