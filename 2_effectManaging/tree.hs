module Tree where

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

{--
GHCi> foldr (:) [] $ PreO tree
[3,1,2,4]
GHCi> foldr (:) [] $ PostO tree
[2,1,4,3]
GHCi> foldr (:) [] $ LevelO tree
[3,1,4,2] 
   3               5
  / \             / \
 1   4           3   6
  \             / \
   2           2   4
              /
             1
--}

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
tree1 = Branch (Branch (Branch (Branch Nil 1 Nil) 2 Nil) 3 (Branch Nil 4 Nil)) 5 (Branch Nil 6 Nil)

instance Foldable Tree where
    foldr f init t = case t of
        Nil -> init
        Branch l v r -> foldr f (v `f` foldr f init r) l

instance Foldable Preorder where
    foldr f init (PreO t) = case t of
        Nil -> init
        Branch l v r -> v `f` foldr f (foldr f init (PreO r)) (PreO l)

instance Foldable Postorder where
    foldr f init (PostO t) = case t of
        Nil -> init
        Branch l v r -> foldr f (foldr f (f v init) (PostO r)) (PostO l)

instance Foldable Levelorder where
    foldr f init tr@(LevelO t) = let t' = helper [t] [] 
                                  in foldr f init t'
helper :: [Tree a] -> [a] -> [a]
helper [] as = as
helper (Nil:xs) as = helper xs as
helper ((Branch l v r):xs) as = helper (xs ++ (l:[r])) (as ++ [v])

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l v r) = Branch (fmap f l) (f v) (fmap f r)

instance Traversable Tree where
    traverse f Nil = pure Nil
    traverse f (Branch l v r) = Branch <$> traverse f l <*> f v <*> traverse f r

