module Main
  ( main,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Set (fromList)
import HW3.Action (HIO (..), HiPermission (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings $ outputStrLn "Type \":q\" to exit" >> loop
  where
    loop :: InputT IO ()
    loop = do
      myInput <- getInputLine "hi> "
      case myInput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          case parse input of
            Right expr -> do
              evaluatedInput <- liftIO $ runHIO (eval expr) (fromList [AllowRead, AllowTime])
              case evaluatedInput of
                Right value -> outputStrLn $ show (prettyValue value)
                Left evalError -> outputStrLn $ show evalError
            Left errors -> outputStrLn $ errorBundlePretty errors
          loop
