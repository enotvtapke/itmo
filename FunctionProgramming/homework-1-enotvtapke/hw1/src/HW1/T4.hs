module HW1.T4
  ( tfoldr,
    treeToList,
  )
where

import HW1.T3

instance Foldable Tree where
  foldr _ b Leaf             = b
  foldr f b (Branch _ l x r) = foldr f (f x (foldr f b r)) l

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr = foldr

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
