module ZipList where

import Control.Applicative (ZipList(ZipList), getZipList)

x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) g a = getZipList $ g <$> ZipList a
(>*<) :: [a -> b] -> [a] -> [b]
(>*<) f a = getZipList $ ZipList f <*> ZipList a
