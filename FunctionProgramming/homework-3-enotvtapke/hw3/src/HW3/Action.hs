{-# LANGUAGE DerivingVia #-}

module HW3.Action
  ( HiPermission (..),
    HIO (..),
    PermissionException (..),
  )
where

import Control.Exception.Base (throwIO)
import Control.Monad (when)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.ByteString as B (readFile, writeFile)
import Data.Sequence (fromList)
import Data.Set (Set)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (getCurrentTime)
import GHC.Exception (Exception)
import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random.Stateful (randomRIO)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Bounded, Enum)

newtype PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

require :: HiPermission -> HIO ()
require p = do
  a <- asks (notElem p)
  when a $ liftHIO $ throwIO $ PermissionRequired p

asks :: (Set HiPermission -> a) -> HIO a
asks f = HIO (return . f)

liftHIO :: IO a -> HIO a
liftHIO m = HIO (const m)

instance HiMonad HIO where
  runAction (HiActionRead path) = do
    require AllowRead
    let relPath = "./" ++ path
    isDir <- liftHIO (doesDirectoryExist relPath)
    if isDir
      then liftHIO $ HiValueList . fromList <$> (map (HiValueString . T.pack) <$> listDirectory relPath)
      else do
        bytes <- liftHIO (B.readFile relPath)
        return $ either (const $ HiValueBytes bytes) HiValueString (decodeUtf8' bytes)
  runAction (HiActionWrite path bytes) = do
    require AllowWrite
    liftHIO $ HiValueNull <$ B.writeFile ("./" ++ path) bytes
  runAction (HiActionMkDir path) = do
    require AllowWrite
    liftHIO $ HiValueNull <$ createDirectory path
  runAction (HiActionChDir path) = do
    require AllowRead
    liftHIO $ HiValueNull <$ setCurrentDirectory path
  runAction HiActionCwd = do
    require AllowRead
    liftHIO $ HiValueString . T.pack <$> getCurrentDirectory
  runAction HiActionNow = do
    require AllowTime
    liftHIO $ HiValueTime <$> getCurrentTime
  runAction (HiActionRand x y) = liftHIO $ HiValueNumber . toRational <$> randomRIO (x, y)
  runAction (HiActionEcho x) = do
    require AllowWrite
    liftHIO $ HiValueNull <$ putStrLn (T.unpack x)
