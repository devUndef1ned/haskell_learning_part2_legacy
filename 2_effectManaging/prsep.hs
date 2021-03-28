module PrsEP where

import Control.Applicative

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP f = PrsEP g where
    g i "" = (i + 1, Left $ "pos " ++ show (i + 1) ++ ": unexpected end of input")
    g i (a:as) | f a       = (i + 1, Right (a, as)) 
               | otherwise = (i + 1, Left $ "pos " ++ show (i + 1) ++ ": unexpected " ++ [a])

charEP c = satisfyEP (== c)
anyEP = satisfyEP (const True)

doubleP [a,b] = (\x y -> [x,y]) <$> charEP a <*> charEP b
tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

instance Functor PrsEP where
    fmap f (PrsEP g) = PrsEP h where
        h i s = fmap (\(v, str) -> (f v, str)) <$> g i s

instance Applicative PrsEP where
    pure a = PrsEP ( \ x y -> (x, Right (a, y)))
    pf <*> pv = PrsEP g where
        g i s = let (j, e1) = runPrsEP pf i s in case e1 of
            Left str -> (j, Left str)
            Right (f, str1) -> let (k, e2) = runPrsEP pv j str1 in case e2 of
                Left str' -> (k, Left str')
                Right (v, str2) -> (k, Right (f v, str2))

instance Alternative PrsEP where
    empty = PrsEP ( \ x y -> (x, Left $ "pos " ++ show x ++ ": empty alternative"))
    PrsEP f1 <|> PrsEP f2 = PrsEP g where
        g i s = let (j, e1) = f1 i s in case e1 of
            r@(Right _) -> (j, r)
            l1@(Left _) -> let (k, e2) = f2 i s in case e2 of
                r@(Right _) -> (k, r)
                l2@(Left _) -> if k > j then (k, l2) else (j, l1)
