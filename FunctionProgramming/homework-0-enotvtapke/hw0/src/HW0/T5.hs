module HW0.T5
  ( Nat,
    nz,
    ns,
    nplus,
    nmult,
    nToNum,
    nFromNatural,
  )
where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns f x = x . f x

nplus :: Nat a -> Nat a -> Nat a
nplus n1 n2 f = n1 f . n2 f

nmult :: Nat a -> Nat a -> Nat a
nmult n1 n2 f = n1 $ n2 f

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural $ n - 1
