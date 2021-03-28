module Result where

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
    fmap f r = case r of
        Ok a -> Ok (f a)
        Error s -> Error s

instance Foldable Result where
    foldMap mF (Ok a) = mF a 
    foldMap _ (Error s) = mempty

instance Traversable Result where
    traverse f (Ok a) = Ok <$> f a
    traverse f (Error s) = pure (Error s)