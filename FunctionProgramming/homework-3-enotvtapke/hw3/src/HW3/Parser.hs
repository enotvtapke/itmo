{-# LANGUAGE GADTs #-}

module HW3.Parser
  ( parse,
  )
where

import Control.Applicative hiding (many, some)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString (ByteString, pack)
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Char as Char
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T (Text, pack)
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..), Labeled, label)
import Text.Megaparsec (MonadParsec (..), Parsec, between, choice, many, manyTill, runParser,
                        satisfy, sepBy, sepBy1, some, try, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (charLiteral, lexeme, scientific, signed, space,
                                                  symbol)
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space >> expr <* eof) ""

expr :: Parser HiExpr
expr = makeExprParser term operatorTable

term :: Parser HiExpr
term =
  try
    ( do
        e <- parens expr <|> list <|> dict <|> value
        args <- (Just <$> some (Right <$> (parens (sepBy expr (symbol ",")) <|> (symbol "." >> (: []) <$> dotParam)) <|> Left <$> symbol "!")) <|> return Nothing
        let withArgs = maybe e (foldl (\x y -> case y of Right b -> HiExprApply x b; Left _ -> HiExprRun x) e) args
        action <- optional $ symbol "!"
        return $ if isJust action then HiExprRun withArgs else withArgs
    )

dotParam :: Parser HiExpr
dotParam = HiExprValue . HiValueString . T.pack <$> (intercalate "-" <$> (lexeme ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'))

list :: Parser HiExpr
list =
  HiExprApply (HiExprValue (HiValueFunction HiFunList))
    <$> between
      (try (symbol "[" >> notFollowedBy (string "#")))
      (symbol "]")
      (lexeme (sepBy expr (symbol ",")))

dict :: Parser HiExpr
dict =
  HiExprDict
    <$> between
      (symbol "{")
      (symbol "}")
      (lexeme (sepBy (((,) <$> expr) <* symbol ":" <*> expr) (symbol ",")))

value :: Parser HiExpr
value =
  HiExprValue
    <$> lexeme
      ( choice
          [ HiValueBytes <$> bytes,
            HiValueBool <$> bool,
            HiValueNumber <$> number,
            HiValueFunction <$> function,
            HiValueString <$> stringLiteral,
            HiValueNull <$ keyword "null",
            HiValueAction HiActionCwd <$ keyword "cwd",
            HiValueAction HiActionNow <$ keyword "now"
          ]
      )

bytes :: Parser ByteString
bytes =
  pack
    <$> between
      (symbol "[#")
      (symbol "#]")
      (lexeme (sepBy byte (try (space1 >> notFollowedBy (string "#")))))
  where
    byte :: Parser Word8
    byte = fromIntegral <$> ((+) . (16 *) . Char.digitToInt <$> hexDigitChar <*> (Char.digitToInt <$> hexDigitChar))

stringLiteral :: Parser T.Text
stringLiteral = T.pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

number :: Parser Rational
number = toRational <$> lexeme (L.signed space L.scientific)

bool :: Parser Bool
bool = (True <$ symbol "true") <|> (False <$ symbol "false")

keyword :: String -> Parser String
keyword s = try $ string s <* notFollowedBy (alphaNumChar <|> char '-')

function :: Parser HiFun
function =
  lexeme
    ( choice
        [ name HiFunMul,
          name HiFunDiv,
          name HiFunAdd,
          name HiFunSub,
          name HiFunNot,
          name HiFunAnd,
          name HiFunOr,
          name HiFunLessThan,
          name HiFunGreaterThan,
          name HiFunEquals,
          name HiFunNotLessThan,
          name HiFunNotGreaterThan,
          name HiFunNotEquals,
          name HiFunIf,
          name HiFunLength,
          name HiFunToUpper,
          name HiFunToLower,
          name HiFunReverse,
          name HiFunTrim,
          name HiFunList,
          name HiFunRange,
          name HiFunFold,
          name HiFunPackBytes,
          name HiFunUnpackBytes,
          name HiFunZip,
          name HiFunUnzip,
          name HiFunEncodeUtf8,
          name HiFunDecodeUtf8,
          name HiFunSerialise,
          name HiFunDeserialise,
          name HiFunRead,
          name HiFunWrite,
          name HiFunMkDir,
          name HiFunChDir,
          name HiFunParseTime,
          name HiFunRand,
          name HiFunEcho,
          name HiFunCount,
          name HiFunKeys,
          name HiFunValues,
          name HiFunInvert
        ]
    )
    <?> "function name"

name :: Labeled a => a -> Parser a
name x = x <$ keyword (HW3.Base.label x)

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryOperator InfixL (try (string "/" <* notFollowedBy (string "="))) HiFunDiv,
      binaryOperator InfixL (string "*") HiFunMul
    ],
    [ binaryOperator InfixL (string "+") HiFunAdd,
      binaryOperator InfixL (string "-") HiFunSub
    ],
    [ binaryOperator InfixN (try $ string "<" <* notFollowedBy (string "=")) HiFunLessThan,
      binaryOperator InfixN (try $ string ">" <* notFollowedBy (string "=")) HiFunGreaterThan,
      binaryOperator InfixN (string ">=") HiFunNotLessThan,
      binaryOperator InfixN (string "<=") HiFunNotGreaterThan,
      binaryOperator InfixN (string "==") HiFunEquals,
      binaryOperator InfixN (string "/=") HiFunNotEquals
    ],
    [binaryOperator InfixR (string "&&") HiFunAnd],
    [binaryOperator InfixR (string "||") HiFunOr]
  ]
  where
    binaryOperator :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> Parser a -> HiFun -> Operator Parser HiExpr
    binaryOperator operator nameParser fun =
      operator ((\x y -> HiExprApply (HiExprValue (HiValueFunction fun)) [x, y]) <$ lexeme nameParser)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

space :: Parser ()
space =
  L.space
    space1
    empty
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space
