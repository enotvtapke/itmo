module HW1.T3
  ( Tree (..),
    tdepth,
    tsize,
    tinsert,
    tmember,
    tFromList,
  )
where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving (Show)

tdepth :: Tree a -> Int
tdepth Leaf                  = 0
tdepth (Branch (h, _) _ _ _) = h

tsize :: Tree a -> Int
tsize Leaf                  = 0
tsize (Branch (_, s) _ _ _) = s

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l x r = Branch (max (tdepth l) (tdepth r) + 1, tsize l + tsize r + 1) l x r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert k Leaf = mkBranch Leaf k Leaf
tinsert k t@(Branch _ l x r)
  | tmember k t = t
  | k < x = balance $ replaceLeft t $ tinsert k l
  | otherwise = balance $ replaceRight t $ tinsert k r

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember k (Branch _ l x r) = case compare k x of
  EQ -> True
  LT -> tmember k l
  GT -> tmember k r

tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf

class AvlTree t where
  bFactor :: t -> Int

  replaceLeft :: t -> t -> t
  replaceRight :: t -> t -> t

  rotateLeft :: t -> t
  rotateRight :: t -> t

  balance :: t -> t

instance AvlTree (Tree a) where
  bFactor Leaf             = 0
  bFactor (Branch _ l _ r) = tdepth r - tdepth l

  replaceLeft (Branch _ _ x r) newLeft = mkBranch newLeft x r

  replaceRight (Branch _ l x _) newRight = mkBranch l x newRight

  rotateLeft a@(Branch _ _ _ b@(Branch _ q _ _)) = replaceLeft b $ replaceRight a q

  rotateRight b@(Branch _ a@(Branch _ _ _ q) _ _) = replaceRight a $ replaceLeft b q

  balance t@(Branch _ l _ r) = case bFactor t of
    2  -> rotateLeft $ if bFactor r < 0 then replaceRight t $ rotateRight r else t
    -2 -> rotateRight $ if bFactor l > 0 then replaceLeft t $ rotateLeft l else t
    _  -> t
