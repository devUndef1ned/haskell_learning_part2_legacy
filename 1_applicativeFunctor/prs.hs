module Prs where

import Data.Char
import Control.Applicative

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

anyChr :: Prs Char
anyChr = Prs fun where
    fun ""     = Nothing 
    fun (c:cs) = Just (c, cs)

instance Functor Prs where
  fmap g pr = Prs g' where
      g' s = let res = runPrs pr s 
              in case res of
                   Nothing      -> Nothing
                   Just (c, cs) -> Just (g c, cs)

instance Applicative Prs where
    pure a = Prs f where
             f s = Just (a, s)

    pFun <*> pVal = Prs $ \s -> 
        do
         (g, s') <- runPrs pFun s
         (aVal, s'') <- runPrs pVal s'
         Just (g aVal, s'')

instance Alternative Prs where
    empty = Prs $ const Nothing
    f <|> g = Prs $ \s -> 
        let fRes = runPrs f s in case fRes of
         a@(Just _)  -> a
         Nothing -> runPrs g s

many :: Prs a -> Prs [a]
many p = (:) <$> p <*> Prs.many p <|> pure []

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> Prs.many p

satisfy p = Prs fun where
    fun "" = Nothing 
    fun (a:as) | p a = Just  (a, as)
               | otherwise = Nothing

nat :: Prs Int
nat = read <$> many1 (satisfy isDigit)

digit :: Prs Int 
digit = digitToInt <$> satisfy isDigit

char c = satisfy (== c)

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

test = runPrs (digitToInt <$> anyChr) "BCD"

