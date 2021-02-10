module PrsE where

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE f where
    f "" = Left "unexpected end of input"
    f (a:as) | p a       = Right (a, as)
             | otherwise = Left $ "unexpected " ++ [a]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
    fmap g (PrsE f) = PrsE $ \s -> do
        (a, s') <- f s 
        Right (g a, s')

instance Applicative PrsE where
    pure x = PrsE $ \s -> Right (x, s)
    pFun <*> pVal = PrsE $ \s -> do
        (fun, s')    <- runPrsE pFun s
        (aVal, s'') <- runPrsE pVal s'
        Right (fun aVal, s'')