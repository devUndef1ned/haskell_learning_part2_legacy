{-# LANGUAGE TypeOperators #-}
module Cmps where

import Data.Functor.Compose

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap h = Cmps . fmap (fmap h) . getCmps

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldMap h = foldMap (foldMap h) . getCmps

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
    traverse h t = Cmps <$> traverse (traverse h) (getCmps t)


-- sequenceA . fmap Compose == Compose . fmap sequenceA . sequenceA 
-- :: (Applicative f, Applicative g, Traversable t) => t (f (g a)) -> Compose f g (t a)
-- 1-st sequenceA :: t (f (g a)) -> f (t (g a) )
-- 2-nd fmap sequenceA :: f ( t (g a)) -> f g (t a)

-- sequenceA :: t (f a) -> f (t a)
-- fmap :: (a -> b) -> f a -> f b

h1 = Compose . fmap sequenceA . sequenceA