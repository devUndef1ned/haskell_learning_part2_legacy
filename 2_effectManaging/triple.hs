module Triple where

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
 foldl f init (Tr a1 a2 a3) = f ( f (f init a1) a2) a3
 foldr f init (Tr a1 a2 a3) = f a1 $ f a2 $ f a3 init

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure a = Tr a a a
    (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

instance Traversable Triple where
    traverse apF (Tr x y z) = Tr <$> apF x <*> apF y <*> apF z 
