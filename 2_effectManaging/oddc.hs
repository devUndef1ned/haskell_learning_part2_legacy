module OddC where

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
 fmap f o = case o of
     (Un a)            -> Un $ f a
     Bi a1 a2 (Un a3)  -> Bi (f a1) (f a2) (Un (f a3))
     Bi a1 a2 a3@Bi {} -> Bi (f a1) (f a2) (fmap f a3)

instance Foldable OddC where
    foldMap f o = case o of
        (Un a)            -> f a
        Bi a1 a2 (Un a3)  -> f a1 <> f a2 <> f a3
        Bi a1 a2 a3@Bi {} -> f a1 <> f a2 <> foldMap f a3

instance Traversable OddC where
    traverse f o = case o of
        (Un a)            -> Un <$> f a
        Bi a1 a2 (Un a3)  -> Bi <$> f a1 <*> f a2 <*> (Un <$> f a3)
        Bi a1 a2 a3@Bi {} -> Bi <$> f a1 <*> f a2 <*> traverse f a3

instance Monad OddC where
    return = Un
    Un a >>= f = f a
    Bi a1 a2 a3 >>= f = concatOC $ Bi (f a1) (f a2) (Un (a3 >>= f))

instance Applicative OddC where
    pure                        = return
    Un f <*> Un v               = Un (f v)
    Un f <*> Bi a1 a2 a3        = let b3 = Un f <*> a3 
                                  in Bi (f a1) (f a2) b3
    Bi f1 f2 f3 <*> Un a        = let b3 = f3 <*> Un a
                                  in Bi (f1 a) (f2 a) b3
    Bi f1 f2 f3 <*> a = concat3OC (Un f1 <*> a) (Un f2 <*> a) (f3 <*> a)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a1) (Un a2) a3 = Bi a1 a2 a3
concat3OC (Un a1) (Bi a2 a3 a4) a5 = Bi a1 a2 (concat3OC (Un a3) a4 a5)
concat3OC (Bi a1 a2 a3) a4 a5 = Bi a1 a2 (concat3OC a3 a4 a5)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi a1 a2 a3) = concat3OC a1 a2 (concatOC a3)

tst1 = Bi 10 20 (Un 30)
tst2 = Bi 1 2 (Bi 3 4 (Un 5))
