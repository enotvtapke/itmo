module HW3.Pretty
  ( prettyValue,
  )
where

import Data.ByteString (ByteString, unpack)
import Data.Foldable (toList)
import qualified Data.Map as M (toList)
import Data.Scientific (fromRationalRepetendUnlimited)
import qualified Data.Text as T (pack)
import GHC.Real (Ratio (..))
import HW3.Base (HiAction (..), HiValue (..), label)
import Numeric (showHex)
import Prettyprinter (Doc, Pretty (..), align, concatWith, dquote, enclose, hsep, pretty, surround,
                      (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty . PrettyHiValue

-- Wrapper for HiValue to eliminate orphan instance during instantiating Pretty for HiValue
newtype PrettyHiValue = PrettyHiValue HiValue

instance Pretty PrettyHiValue where
  pretty (PrettyHiValue hiValue) = case hiValue of
    (HiValueNumber (x :% 1)) -> pretty x
    (HiValueNumber x@(p :% q)) -> case fromRationalRepetendUnlimited x of
      (a, Nothing) -> pretty $ show a
      (_, Just _) -> case quotRem p q of
        (0, x1) -> pretty x1 <> pretty "/" <> pretty q
        (x1, x2) -> pretty x1 <+> pretty (if x < 0 then "-" else "+") <+> pretty (abs x2) <> pretty "/" <> pretty q
    (HiValueFunction fun) -> pretty $ label fun
    (HiValueBool bool) -> if bool then pretty "true" else pretty "false"
    HiValueNull -> pretty "null"
    (HiValueString str) -> pretty $ show str
    (HiValueList list) ->
      enclose (pretty "[ ") (pretty " ]") $
        align $ concatWith (surround (pretty ", ")) $ map (pretty . PrettyHiValue) (toList list)
    (HiValueBytes bytes) -> pretty $ PrettyByteString bytes
    (HiValueTime time) -> pretty "parse-time(" <> dquote <> pretty (show time) <> dquote <> pretty ")"
    (HiValueDict dict) ->
      enclose (pretty "{ ") (pretty " }") $
        align $
          concatWith (surround (pretty ", ")) $
            map (\(x, y) -> pretty (PrettyHiValue x) <> pretty ": " <> pretty (PrettyHiValue y)) $ M.toList dict
    (HiValueAction action) -> pretty $ PrettyHiAction action

newtype PrettyByteString = PrettyByteString ByteString

instance Pretty PrettyByteString where
  pretty (PrettyByteString s) =
    enclose (pretty "[# ") (pretty " #]") $
      align $ hsep $ map (\x -> (if x < 16 then pretty "0" else mempty) <> pretty (showHex x "")) (unpack s)

newtype PrettyHiAction = PrettyHiAction HiAction

instance Pretty PrettyHiAction where
  pretty (PrettyHiAction hiAction) = case hiAction of
    (HiActionRead path) -> pretty "read" <> args [show path]
    (HiActionWrite path content) ->
        pretty "write"
          <> args [PrettyHiValue $ HiValueString $ T.pack path, PrettyHiValue $ HiValueBytes content]
    (HiActionMkDir path) -> pretty "mkdir" <> args [show path]
    (HiActionChDir path) -> pretty "cd" <> args [show path]
    HiActionCwd -> pretty "cwd"
    HiActionNow -> pretty "now"
    (HiActionRand x y) -> pretty "rand" <> args [x, y]
    (HiActionEcho s) -> pretty "echo" <> args [show s]

args :: Pretty a => [a] -> Doc ann
args l = enclose (pretty "(") (pretty ")") (concatWith (surround (pretty ", ")) (map pretty l))
