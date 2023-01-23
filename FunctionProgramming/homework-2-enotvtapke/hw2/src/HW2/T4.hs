module HW2.T4
  ( State (..),
    Prim (..),
    Expr (..),
    eval,
  )
where

import Control.Monad
import HW2.T1

newtype State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S (\x -> let (a :# s) = g x in f a :# s)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S (\s1 -> let a :# s2 = f s1 in (let S g = a in (let b :# s3 = g s2 in b :# s3)))

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum
  deriving (Show)

data Expr = Val Double | Op (Prim Expr) deriving (Show)

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) =
  do
    a <- eval x
    b <- eval y
    modifyState (\s -> Add a b : s)
    return $ a + b
eval (Op (Sub x y)) =
  do
    a <- eval x
    b <- eval y
    modifyState (\s -> Sub a b : s)
    return $ a - b
eval (Op (Mul x y)) =
  do
    a <- eval x
    b <- eval y
    modifyState (\s -> Mul a b : s)
    return $ a * b
eval (Op (Div x y)) =
  do
    a <- eval x
    b <- eval y
    modifyState (\s -> Div a b : s)
    return $ a / b
eval (Op (Abs x)) =
  do
    a <- eval x
    modifyState (\s -> Abs a : s)
    return $ abs a
eval (Op (Sgn x)) =
  do
    a <- eval x
    modifyState (\s -> Sgn a : s)
    return $ signum a
