{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( parseExpr,
    evalExpr,
  )
where

import Data.Char (isDigit)
import GHC.Base (Alternative (..))
import GHC.Natural (Natural)
import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (EvaluationError, ExceptState (..), eval)

newtype ParseError = ErrorAtPos Natural deriving (Show)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) s = case runES es (0, s) of
  Success (a :# _) -> Success a
  Error e          -> Error e

pChar :: Parser Char
pChar = P $
  ES $ \(pos, s) ->
    case s of
      []       -> Error (ErrorAtPos pos)
      (c : cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P (ES (\_ -> Error $ ErrorAtPos 0))

instance Alternative Parser where
  empty = parseError
  P (ES p1) <|> P (ES p2) =
    P $
      ES
        ( \s -> case p1 s of
            Success x -> Success x
            Error _   -> p2 s
        )

instance Semigroup a => Semigroup (Parser a) where
  a <> b = a >>= (\s -> (s <>) <$> b)

instance Monoid a => Monoid (Parser a) where
  mempty = return mempty

pEof :: Parser ()
pEof = P $
  ES $ \(pos, s) ->
    case s of
      [] -> Success (() :# (pos, s))
      _  -> Error (ErrorAtPos pos)

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ do
  e <- expr
  pEof
  return e

evalExpr :: String -> Except EvaluationError (Annotated [Prim Double] Double)
evalExpr s = case parseExpr s of
  Success e -> runES (eval e) []
  Error e   -> error $ show e

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $
  ES $ \(pos, s) ->
    case s of
      []       -> Error (ErrorAtPos pos)
      (c : cs) -> if f c then Success (c :# (pos + 1, cs)) else Error (ErrorAtPos pos)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string []       = return ""
string (c : cs) = char c >>= (\s -> (s :) <$> string cs)

oneOf :: [String] -> Parser String
oneOf = foldr ((<|>) . string) parseError

digit :: Parser Char
digit = satisfy isDigit

maybeParser :: Monoid a => Parser a -> Parser a
maybeParser p = p <|> return mempty

maybeString :: String -> Parser String
maybeString s = maybeParser $ string s

float :: Parser Expr
float = do
  f <- maybeString "-" <> some digit <> maybeParser (string "." <> some digit)
  return $ Val (read f :: Double)

inBrackets :: Parser Expr -> Parser Expr
inBrackets p = do
  _ <- string "("
  e <- p
  _ <- string ")"
  return e

inSpaces :: Parser Expr -> Parser Expr
inSpaces p = do
  _ <- many $ char ' '
  e <- p
  _ <- many $ char ' '
  return e

fl :: Parser Expr
fl = inSpaces $ inBrackets expr <|> float

expr :: Parser Expr
expr = inSpaces $ do
  x <- term
  expr' x

expr' :: Expr -> Parser Expr
expr' x =
  ( do
      op <- oneOf ["+", "-"]
      y <- term
      expr' $ case op of
        "+" -> Op (Add x y)
        "-" -> Op (Sub x y)
        _   -> undefined
  )
    <|> (string "" >> return x)

term :: Parser Expr
term = inSpaces $ do
  x <- fl
  term' x

term' :: Expr -> Parser Expr
term' x =
  ( do
      op <- oneOf ["*", "/"]
      y <- fl
      term' $ case op of
        "*" -> Op (Mul x y)
        "/" -> Op (Div x y)
        _   -> undefined
  )
    <|> (string "" >> return x)
