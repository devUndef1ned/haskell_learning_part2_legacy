{-# LANGUAGE TypeOperators #-}
module Cmps where

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  (<*>) = undefined

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (2, ('a', True))

b :: B t
b = Cmps (False, id, Right 5)

c :: C
c  = Cmps f where
   f True = \x -> x ^ 2
   f False = \x -> x - 2

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 c0 = getCmps <$> getCmps c0
                 

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 c0 = fmap getCmps . getCmps <$> getCmps c0
