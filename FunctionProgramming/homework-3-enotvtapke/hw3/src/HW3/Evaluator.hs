{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module HW3.Evaluator
  ( eval,
  )
where

import Codec.Compression.Zlib (CompressParams (..), bestCompression, compressWith, decompress,
                               defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import qualified Data.ByteString as B (ByteString, append, drop, index, length, pack, reverse, take,
                                       unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.List (nub)
import qualified Data.Map.Strict as M (Map, elems, fromList, fromListWith, keys, lookup, mapKeys,
                                       mapMaybe, toList)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as S (drop, fromList, lookup, reverse, singleton, take)
import qualified Data.Text as T (Text, append, cons, drop, index, length, reverse, singleton, strip,
                                 take, toLower, toUpper, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (addUTCTime, diffUTCTime)
import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction),
                 HiValue (..))
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue value) = return $ Right value
eval (HiExprApply expr args) = do
  e <- eval expr
  case e of
    x@(Left _) -> return x
    -- Short circuit evaluation
    Right (HiValueFunction HiFunIf) -> do
      case args of
        [cond, x, y] -> do
          c <- eval cond
          case c of
            l@(Left _)            -> return l
            Right (HiValueBool b) -> if b then eval x else eval y
            Right HiValueNull     -> eval y
            Right _               -> return $ Left HiErrorInvalidArgument
        _ -> return $ Left HiErrorArityMismatch
    Right (HiValueFunction HiFunAnd) -> do
      case args of
        [x, y] -> do
          a <- eval x
          case a of
            l@(Left _)                -> return l
            Right (HiValueBool False) -> return a
            Right HiValueNull         -> return a
            Right _                   -> eval y
        _ -> return $ Left HiErrorArityMismatch
    Right (HiValueFunction HiFunOr) -> do
      case args of
        [x, y] -> do
          a <- eval x
          case a of
            l@(Left _)                -> return l
            Right (HiValueBool False) -> eval y
            Right HiValueNull         -> eval y
            Right _                   -> return a
        _ -> return $ Left HiErrorArityMismatch
    Right (HiValueFunction x) -> evalCallable x args
    Right (HiValueString x) -> evalCallable x args
    Right (HiValueList x) -> evalCallable x args
    Right (HiValueBytes x) -> evalCallable x args
    Right (HiValueDict x) -> evalCallable x args
    Right _ -> return $ Left HiErrorInvalidFunction
eval (HiExprRun expr) = do
  action <- eval expr
  case action of
    l@(Left _)              -> return l
    Right (HiValueAction a) -> Right <$> runAction a
    Right _                 -> return $ Left HiErrorInvalidFunction
eval (HiExprDict dict) = do
  keys <- mapM (eval . fst) dict
  values <- mapM (eval . snd) dict
  return $ do
    a <- sequence keys
    b <- sequence values
    return $ HiValueDict $ M.fromList $ zip a b

evalCallable :: (HiMonad m, Callable f) => f -> [HiExpr] -> m (Either HiError HiValue)
evalCallable f args = do
  evalArgs <- mapM eval args
  return $ do
    values <- sequence evalArgs
    call f values

-- Class for data types that can be called with HiValues as arguments
class Callable e where
  call :: e -> [HiValue] -> Either HiError HiValue

-- Class for data types that supports indexing and slices
class HiIterable a where
  wrap :: a -> HiValue
  takeFirst :: Int -> a -> a
  dropFirst :: Int -> a -> a
  len :: a -> Int
  index :: a -> Int -> HiValue
  safeIndex :: a -> HiValue -> Either HiError HiValue
  safeIndex s (HiValueNumber x) = do
    i <- toInt x
    if 0 <= i && i < len s
      then return $ index s i
      else return HiValueNull
  safeIndex _ _ = Left HiErrorInvalidArgument
  slice :: a -> HiValue -> HiValue -> Either HiError HiValue
  slice s (HiValueNumber x) (HiValueNumber y) = do
    a <- toInt x
    b <- toInt y
    l <- Right $ if a < 0 then max (len s + a) 0 else a
    r <- Right $ if b < 0 then len s + b else b
    return $ wrap $ (takeFirst (r - l) . dropFirst l) s
  slice s (HiValueNumber x) HiValueNull = slice s (HiValueNumber x) (HiValueNumber $ toRational $ len s)
  slice s HiValueNull (HiValueNumber x) = slice s (HiValueNumber 0) (HiValueNumber x)
  slice s HiValueNull HiValueNull = Right $ wrap s
  slice _ _ _ = Left HiErrorInvalidArgument

instance HiIterable (Seq HiValue) where
  wrap = HiValueList
  takeFirst = S.take
  dropFirst = S.drop
  len = length
  index s i = fromMaybe HiValueNull (S.lookup i s)

instance HiIterable T.Text where
  wrap = HiValueString
  takeFirst = T.take
  dropFirst = T.drop
  len = T.length
  index s i = HiValueString $ T.singleton $ T.index s i

instance HiIterable B.ByteString where
  wrap = HiValueBytes
  takeFirst = B.take
  dropFirst = B.drop
  len = B.length
  index s i = HiValueNumber $ toRational $ B.index s i

instance Callable (Seq HiValue) where
  call s [x]    = safeIndex s x
  call s [x, y] = slice s x y
  call _ _      = Left HiErrorArityMismatch

instance Callable T.Text where
  call s [x]    = safeIndex s x
  call s [x, y] = slice s x y
  call _ _      = Left HiErrorArityMismatch

instance Callable B.ByteString where
  call s [x]    = safeIndex s x
  call s [x, y] = slice s x y
  call _ _      = Left HiErrorArityMismatch

instance Callable (M.Map HiValue HiValue) where
  call m [x] = case M.lookup x m of
    (Just e) -> Right e
    _        -> Right HiValueNull
  call _ _ = Left HiErrorArityMismatch

instance Callable HiFun where
  call HiFunAdd [HiValueNumber x, HiValueNumber y] = Right $ HiValueNumber $ x + y
  call HiFunAdd [HiValueString x, HiValueString y] = Right $ HiValueString $ T.append x y
  call HiFunAdd [HiValueList x, HiValueList y] = Right $ HiValueList $ x >< y
  call HiFunAdd [HiValueBytes x, HiValueBytes y] = Right $ HiValueBytes $ B.append x y
  call HiFunAdd [HiValueTime x, HiValueNumber y] = Right $ HiValueTime $ addUTCTime (fromRational y) x
  call HiFunSub [HiValueNumber x, HiValueNumber y] = Right $ HiValueNumber $ x - y
  call HiFunSub [HiValueTime x, HiValueTime y] = Right $ HiValueNumber $ toRational $ diffUTCTime x y
  call HiFunMul [HiValueNumber x, HiValueNumber y] = Right $ HiValueNumber $ x * y
  call HiFunMul [HiValueString x, HiValueNumber y] = do
    times <- toInt y
    if times <= 0 then Left HiErrorInvalidArgument else Right $ HiValueString $ stimes times x
  call HiFunMul [HiValueList x, HiValueNumber y] = do
    times <- toInt y
    if times <= 0 then Left HiErrorInvalidArgument else Right $ HiValueList $ stimes times x
  call HiFunMul [HiValueBytes x, HiValueNumber y] = do
    times <- toInt y
    if times <= 0 then Left HiErrorInvalidArgument else Right $ HiValueBytes $ stimes times x
  call HiFunDiv [HiValueNumber x, HiValueNumber y] =
    if y == 0 then Left HiErrorDivideByZero else Right $ HiValueNumber $ x / y
  call HiFunDiv [HiValueString x, HiValueString y] = Right $ HiValueString $ T.append x $ T.cons '/' y
  call HiFunNot [HiValueBool x] = Right $ HiValueBool $ not x
  call HiFunAnd [HiValueBool x, HiValueBool y] = Right $ HiValueBool $ x && y
  call HiFunOr [HiValueBool x, HiValueBool y] = Right $ HiValueBool $ x || y
  call HiFunLessThan [x, y] = Right $ HiValueBool $ x < y
  call HiFunEquals [x, y] = Right $ HiValueBool $ x == y
  call HiFunGreaterThan [x, y] = Right $ HiValueBool $ x > y
  call HiFunNotEquals [x, y] = Right $ HiValueBool $ x /= y
  call HiFunNotLessThan [x, y] = Right $ HiValueBool $ x >= y
  call HiFunNotGreaterThan [x, y] = Right $ HiValueBool $ x <= y
  call HiFunIf [HiValueBool cond, x, y] = Right $ if cond then x else y
  call HiFunLength [HiValueString s] = Right $ HiValueNumber $ toRational $ T.length s
  call HiFunLength [HiValueList s] = Right $ HiValueNumber $ toRational $ length s
  call HiFunLength [HiValueBytes s] = Right $ HiValueNumber $ toRational $ B.length s
  call HiFunToUpper [HiValueString s] = Right $ HiValueString $ T.toUpper s
  call HiFunToLower [HiValueString s] = Right $ HiValueString $ T.toLower s
  call HiFunTrim [HiValueString s] = Right $ HiValueString $ T.strip s
  call HiFunReverse [HiValueString s] = Right $ HiValueString $ T.reverse s
  call HiFunReverse [HiValueList s] = Right $ HiValueList $ S.reverse s
  call HiFunReverse [HiValueBytes s] = Right $ HiValueBytes $ B.reverse s
  call HiFunList elems = Right $ HiValueList $ S.fromList elems
  call HiFunRange [HiValueNumber x, HiValueNumber y] = Right $ HiValueList $ S.fromList $ map HiValueNumber [x .. y]
  call HiFunFold [HiValueFunction f, HiValueList (el :<| els)] =
    foldl
      ( \x y -> case x of
          e@(Left _) -> e
          Right e    -> call f [e, y]
      )
      (Right el)
      els
  call HiFunPackBytes [HiValueList l] = do
    bytes <-
      traverse
        ( \case
            HiValueNumber x -> do
              a <- toInt x
              if 0 <= x && x < 256 then Right (fromIntegral a) else Left HiErrorInvalidArgument
            _ -> Left HiErrorInvalidArgument
        )
        l
    return $ HiValueBytes $ B.pack (toList bytes)
  call HiFunUnpackBytes [HiValueBytes b] = Right $ HiValueList $ S.fromList $ map (HiValueNumber . toRational) (B.unpack b)
  call HiFunEncodeUtf8 [HiValueString s] = Right $ HiValueBytes $ encodeUtf8 s
  call HiFunDecodeUtf8 [HiValueBytes b] = case decodeUtf8' b of
    Right x -> Right $ HiValueString x
    Left _  -> Right HiValueNull
  call HiFunZip [HiValueBytes b] =
    Right $ HiValueBytes $ toStrict (compressWith defaultCompressParams {compressLevel = bestCompression} (fromStrict b))
  call HiFunUnzip [HiValueBytes b] =
    Right $ HiValueBytes $ toStrict (decompress (fromStrict b))
  call HiFunSerialise [val] = Right $ HiValueBytes $ toStrict $ serialise val
  call HiFunDeserialise [HiValueBytes b] = case deserialiseOrFail (fromStrict b) of
    Right x -> Right x
    Left _  -> Right HiValueNull
  call HiFunRead [HiValueString s] = Right $ HiValueAction $ HiActionRead $ T.unpack s
  call HiFunWrite [HiValueString x, HiValueString y] = Right $ HiValueAction $ HiActionWrite (T.unpack x) (encodeUtf8 y)
  call HiFunWrite [HiValueString x, HiValueBytes y] = Right $ HiValueAction $ HiActionWrite (T.unpack x) y
  call HiFunMkDir [HiValueString x] = Right $ HiValueAction $ HiActionMkDir (T.unpack x)
  call HiFunChDir [HiValueString x] = Right $ HiValueAction $ HiActionChDir (T.unpack x)
  call HiFunParseTime [HiValueString x] = Right $ maybe HiValueNull HiValueTime $ readMaybe (T.unpack x)
  call HiFunRand [HiValueNumber x, HiValueNumber y] = do
    a <- toInt x
    b <- toInt y
    return $ HiValueAction $ HiActionRand a b
  call HiFunEcho [HiValueString x] = Right $ HiValueAction $ HiActionEcho x
  call HiFunKeys [HiValueDict dict] = Right $ HiValueList $ S.fromList $ M.keys dict
  call HiFunValues [HiValueDict dict] = Right $ HiValueList $ S.fromList $ M.elems dict
  call HiFunCount [HiValueList l] = Right $ HiValueDict $ listToMap $ toList l
  call HiFunCount [HiValueBytes l] = Right $ HiValueDict $ M.mapKeys (HiValueNumber . toRational) $ listToMap $ B.unpack l
  call HiFunCount [HiValueString l] = Right $ HiValueDict $ M.mapKeys (HiValueString . T.singleton) $ listToMap $ T.unpack l
  call HiFunInvert [HiValueDict dict] =
    Right $
      HiValueDict $
        M.mapMaybe (Just . HiValueList) $ M.fromListWith (><) $ map (\(x, y) -> (y, S.singleton x)) $ M.toList dict
  call HiFunAdd [_, _] = Left HiErrorInvalidArgument
  call HiFunSub [_, _] = Left HiErrorInvalidArgument
  call HiFunMul [_, _] = Left HiErrorInvalidArgument
  call HiFunDiv [_, _] = Left HiErrorInvalidArgument
  call HiFunNot [_] = Left HiErrorInvalidArgument
  call HiFunAnd [_, _] = Left HiErrorInvalidArgument
  call HiFunOr [_, _] = Left HiErrorInvalidArgument
  call HiFunIf [_, _, _] = Left HiErrorInvalidArgument
  call HiFunLength [_] = Left HiErrorInvalidArgument
  call HiFunToUpper [_] = Left HiErrorInvalidArgument
  call HiFunToLower [_] = Left HiErrorInvalidArgument
  call HiFunTrim [_] = Left HiErrorInvalidArgument
  call HiFunReverse [_] = Left HiErrorInvalidArgument
  call HiFunRange [_, _] = Left HiErrorInvalidArgument
  call HiFunFold [_, _] = Left HiErrorInvalidArgument
  call HiFunPackBytes [_] = Left HiErrorInvalidArgument
  call HiFunUnpackBytes [_] = Left HiErrorInvalidArgument
  call HiFunEncodeUtf8 [_] = Left HiErrorInvalidArgument
  call HiFunDecodeUtf8 [_] = Left HiErrorInvalidArgument
  call HiFunZip [_] = Left HiErrorInvalidArgument
  call HiFunUnzip [_] = Left HiErrorInvalidArgument
  call HiFunDeserialise [_] = Left HiErrorInvalidArgument
  call HiFunRead [_] = Left HiErrorInvalidArgument
  call HiFunWrite [_, _] = Left HiErrorInvalidArgument
  call HiFunMkDir [_] = Left HiErrorInvalidArgument
  call HiFunChDir [_] = Left HiErrorInvalidArgument
  call HiFunParseTime [_] = Left HiErrorInvalidArgument
  call HiFunRand [_, _] = Left HiErrorInvalidArgument
  call HiFunEcho [_] = Left HiErrorInvalidArgument
  call HiFunKeys [_] = Left HiErrorInvalidArgument
  call HiFunValues [_] = Left HiErrorInvalidArgument
  call HiFunCount [_] = Left HiErrorInvalidArgument
  call HiFunInvert [_] = Left HiErrorInvalidArgument
  call HiFunAdd _ = Left HiErrorArityMismatch
  call HiFunSub _ = Left HiErrorArityMismatch
  call HiFunMul _ = Left HiErrorArityMismatch
  call HiFunDiv _ = Left HiErrorArityMismatch
  call HiFunNot _ = Left HiErrorArityMismatch
  call HiFunAnd _ = Left HiErrorArityMismatch
  call HiFunOr _ = Left HiErrorArityMismatch
  call HiFunLessThan _ = Left HiErrorArityMismatch
  call HiFunEquals _ = Left HiErrorArityMismatch
  call HiFunGreaterThan _ = Left HiErrorArityMismatch
  call HiFunNotEquals _ = Left HiErrorArityMismatch
  call HiFunNotLessThan _ = Left HiErrorArityMismatch
  call HiFunNotGreaterThan _ = Left HiErrorArityMismatch
  call HiFunIf _ = Left HiErrorArityMismatch
  call HiFunLength _ = Left HiErrorArityMismatch
  call HiFunToUpper _ = Left HiErrorArityMismatch
  call HiFunToLower _ = Left HiErrorArityMismatch
  call HiFunTrim _ = Left HiErrorArityMismatch
  call HiFunReverse _ = Left HiErrorArityMismatch
  call HiFunRange _ = Left HiErrorArityMismatch
  call HiFunFold _ = Left HiErrorArityMismatch
  call HiFunPackBytes _ = Left HiErrorArityMismatch
  call HiFunUnpackBytes _ = Left HiErrorArityMismatch
  call HiFunEncodeUtf8 _ = Left HiErrorArityMismatch
  call HiFunDecodeUtf8 _ = Left HiErrorArityMismatch
  call HiFunZip _ = Left HiErrorArityMismatch
  call HiFunUnzip _ = Left HiErrorArityMismatch
  call HiFunSerialise _ = Left HiErrorArityMismatch
  call HiFunDeserialise _ = Left HiErrorArityMismatch
  call HiFunRead _ = Left HiErrorArityMismatch
  call HiFunWrite _ = Left HiErrorArityMismatch
  call HiFunMkDir _ = Left HiErrorArityMismatch
  call HiFunChDir _ = Left HiErrorArityMismatch
  call HiFunParseTime _ = Left HiErrorArityMismatch
  call HiFunRand _ = Left HiErrorArityMismatch
  call HiFunEcho _ = Left HiErrorArityMismatch
  call HiFunKeys _ = Left HiErrorArityMismatch
  call HiFunValues _ = Left HiErrorArityMismatch
  call HiFunCount _ = Left HiErrorArityMismatch
  call HiFunInvert _ = Left HiErrorArityMismatch

-- Helper functions for evaluation
toInt :: Rational -> Either HiError Int
toInt x =
  if denominator x == 1
    then Right $ fromIntegral $ numerator x
    else Left HiErrorInvalidArgument

listToMap :: (Ord a) => [a] -> M.Map a HiValue
listToMap l = do
  let keys = nub l
  let values = map (count l) keys
  M.fromList $ zip keys $ map (HiValueNumber . toRational) values
  where
    count :: Eq a => [a] -> a -> Int
    count ls e = sum (map (\x -> if x == e then 1 else 0) ls)
