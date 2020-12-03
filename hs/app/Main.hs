module Main where

import Import

import Data.String        (String)
import Data.Text          (pack)
import System.Environment (getArgs)
import System.Posix.Env   (getEnvDefault)
import Text.Printf        (printf)

import qualified P01
import qualified P02
import qualified P03

main :: IO ()
main = getNumber >>= \case
  Left err -> die $ "Could not determine example number: " <> pack err
  Right n -> do
    input <- readFile $ "../inputs/" <> printf "%02d" n
    case n of
      1 -> P01.solve input
      2 -> P02.solve input
      3 -> P03.solve input
      _ -> die $ "Not implemented: #" <> show n

getNumber :: IO (Either String Int)
getNumber = fmap readEither readNumber
  where
    readNumber :: IO String
    readNumber = getArgs >>= \case
      arg : _ -> return arg
      _       -> getEnvDefault "N" "1"
