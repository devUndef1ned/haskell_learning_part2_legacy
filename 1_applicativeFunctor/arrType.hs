module ArrType where

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap g (Arr2 f) = Arr2 $ \e1 e2 -> g $ f e1 e2

instance Functor (Arr3 e1 e2 e3) where
  fmap g (Arr3 f) = Arr3 $ \e1 e2 e3 -> g $ f e1 e2 e3  

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 $ \e1 e2 -> x
  (<*>) (Arr2 f) (Arr2 g) = Arr2 $ \e1 e2 -> f e1 e2 $ g e1 e2

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 $ \e1 e2 e3 -> x
  (<*>) (Arr3 f) (Arr3 g) = Arr3 $ \e1 e2 e3 -> f e1 e2 e3 $ g e1 e2 e3 