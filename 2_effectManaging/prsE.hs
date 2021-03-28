module PrsE where

import Control.Applicative
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE f where
    f "" = Left "unexpected end of input"
    f (a:as) | p a       = Right (a, as)
             | otherwise = Left $ "unexpected " ++ [a]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
    fmap f p = PrsE $ \s -> do
        (a, str) <- runPrsE p s
        return (f a, str)

instance Applicative PrsE where
    pure a = PrsE $ \x -> Right (a, x)
    f <*> p = PrsE $ \s -> do
        (a, str) <- runPrsE p s
        (g, str') <- runPrsE f str
        return (g a, str')

instance Monad PrsE where
  p >>= f = PrsE $ \s -> do
      (a, s') <- runPrsE p s
      (b, str) <- runPrsE (f a) s'
      return (b, str)