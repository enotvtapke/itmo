{-# LANGUAGE LambdaCase #-}

module HW1.T6 (mcat, epart) where

import Data.Maybe (fromMaybe)
import Data.Monoid ()

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap $ fromMaybe mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart =
  foldMap
    ( \case
        Left x  -> (x, mempty)
        Right x -> (mempty, x)
    )
