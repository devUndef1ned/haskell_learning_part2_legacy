{-# LANGUAGE TypeOperators #-}
module Fldbl where

import Data.Monoid

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldMap fm = foldMap (foldMap fm) . getCmps