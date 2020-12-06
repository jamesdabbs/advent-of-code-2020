module P06 where

import Import

import Data.Attoparsec.Text (letter, many1)
import qualified Data.Set as Set

type Group = [Set Char]

solve :: Text -> IO ()
solve input = do
  groups <- parse input parser

  print $ sum $ map anyAnswered groups
  print $ sum $ map allAnswered groups

parser :: Parser [Group]
parser = (line `sepBy` "\n") `sepBy` "\n\n"
  where
    line = Set.fromList <$> many1 letter

anyAnswered :: Group -> Int
anyAnswered = Set.size . Set.unions

allAnswered :: Group -> Int
allAnswered (g:gs) = Set.size $ foldl' Set.intersection g gs
allAnswered [] = 0