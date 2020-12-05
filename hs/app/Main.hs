module Main where

import Data.Text (pack)
import Import
import Problems (run)
import System.Posix.Env (getEnvDefault)
import Text.Printf (printf)

main :: IO ()
main =
  getNumber >>= \case
    Left err -> die $ "Could not determine example number: " <> pack err
    Right n -> do
      input <- readFile $ "../inputs/" <> printf "%02d" n
      run n input

getNumber :: IO (Either String Int)
getNumber = fmap readEither readNumber
  where
    readNumber :: IO String
    readNumber =
      getArgs >>= \case
        arg : _ -> return arg
        _ -> getEnvDefault "N" "1"
