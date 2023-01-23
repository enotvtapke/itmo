module HW0.T4
  ( repeat',
    map',
    fac,
    fib,
  )
where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a] -- behaves like Data.List.repeat
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b] -- behaves like Data.List.map
map' f =
  fix
    ( \r x -> case x of
        []       -> []
        (a : as) -> f a : r as
    )

--fibn :: Natural -> Natural -- computes the n-th Fibonacci number
--fibn 0 = 0
--fibn 1 = 1
--fibn n = fib (n - 1) + fib (n - 2)

fib :: Natural -> Natural -- computes the n-th Fibonacci number
fib =
  fix
    ( \r x -> case x of
        0 -> 0
        1 -> 1
        n -> r (n - 1) + r (n - 2)
    )

fac :: Natural -> Natural -- computes the factorial
fac = fix (\r n -> if n <= 1 then 1 else n * r (n -1))
