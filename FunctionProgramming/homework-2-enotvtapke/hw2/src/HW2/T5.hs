module HW2.T5
  ( ExceptState (..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    EvaluationError (..),
    eval,
  )
where

import Control.Monad
import HW2.T1
import HW2.T4 (Expr (..), Prim (..))

data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) =
  ES
    ( \s -> case g s of
        Success (a :# b) -> Success (f a :# b)
        Error e          -> Error e
    )

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) =
  ES
    ( \s -> case f s of
        Success ((ES g) :# s1) -> g s1
        Error x                -> Error x
    )

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero deriving (Show)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) =
  do
    a <- eval x
    b <- eval y
    modifyExceptState (\s -> Add a b : s)
    return $ a + b
eval (Op (Sub x y)) =
  do
    a <- eval x
    b <- eval y
    modifyExceptState (\s -> Sub a b : s)
    return $ a - b
eval (Op (Mul x y)) =
  do
    a <- eval x
    b <- eval y
    modifyExceptState (\s -> Mul a b : s)
    return $ a * b
eval (Op (Div x y)) =
  do
    a <- eval x
    b <- eval y
    when (b == 0) $ throwExceptState DivideByZero
    modifyExceptState (\s -> Div a b : s)
    return $ a / b
eval (Op (Abs x)) =
  do
    a <- eval x
    modifyExceptState (\s -> Abs a : s)
    return $ abs a
eval (Op (Sgn x)) =
  do
    a <- eval x
    modifyExceptState (\s -> Sgn a : s)
    return $ signum a
