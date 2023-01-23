module HW1.T5
  ( splitOn,
    joinWith,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)), cons)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn d a = fmap reverse (_splitOn d a [])
  where
    _splitOn :: Eq a => a -> [a] -> [a] -> NonEmpty [a]
    _splitOn _ [] [] = [] :| []
    _splitOn _ [] acc = acc :| []
    _splitOn d (x : xs) acc = if d == x then cons acc (_splitOn d xs []) else _splitOn d xs (x : acc)

joinWith :: a -> NonEmpty [a] -> [a]
joinWith d (x :| xs) = concat $ x : map (d :) xs
