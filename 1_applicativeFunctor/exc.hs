module ApplicativeFunctor where

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap g f = Arr2 $ \e1 e2 -> g (getArr2 f e1 e2)

instance Functor (Arr3 e1 e2 e3) where
  fmap g f = Arr3 $ \e1 e2 e3 -> g (getArr3 f e1 e2 e3)