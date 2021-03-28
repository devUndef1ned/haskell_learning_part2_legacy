module TreePost where

import Data.Traversable (fmapDefault, foldMapDefault)

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

{--
instance Foldable Postorder where
    foldr f init (PostO t) = case t of
        Nil -> init
        Branch l v r -> foldr f (foldr f (f v init) (PostO r)) (PostO l)

        4
       / \
      2   5
     / \
    1   3

    [1,3,2,5,4]
--}

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

instance Functor Tree where
  fmap = fmapDefault

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse f (Branch l v r) = flip . Branch <$> traverse f l <*> traverse f r <*> f v

instance Foldable Tree where
  foldMap = foldMapDefault