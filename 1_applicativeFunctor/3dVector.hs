module ThreeDVector where

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap g (Tr a1 a2 a3) = Tr (g a1) (g a2) (g a3)

instance Applicative Triple where
    pure a = Tr a a a
    Tr f g y <*> Tr a1 a2 a3 = Tr (f a1) (g a2) (y a3)