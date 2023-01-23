module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    ncmp,
    nFromNatural,
    nToNum,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Data.Maybe (fromMaybe)
import GHC.Natural (Natural)

data N = Z | S N deriving (Show)

nplus :: N -> N -> N -- addition
nplus Z x     = x
nplus (S n) y = nplus n (S y)

nmult :: N -> N -> N -- multiplication
nmult _ Z     = Z
nmult x (S Z) = x
nmult x (S n) = nplus x $ nmult x n

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)
nsub n Z         = Just n
nsub Z _         = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp x y = case nsub x y of
  Just Z  -> EQ
  Nothing -> LT
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural $ n - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = nToNum x + 1

nEven :: N -> Bool
nEven Z     = True
nEven (S x) = not $ nEven x

nOdd :: N -> Bool
nOdd x = not $ nEven x

ndiv :: N -> N -> N
ndiv _ Z = undefined
ndiv x y = case nsub x y of
  Nothing -> Z
  Just n  -> S (ndiv n y)

nmod :: N -> N -> N
nmod x y = fromMaybe undefined (nsub x $ nmult y $ ndiv x y)
