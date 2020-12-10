module P06 where

import Data.Attoparsec.Text (letter, many1)
import qualified Data.Set as Set
import Import

type Group = NonEmpty (Set Char)

solution :: Solution [Group]
solution = solve parser $ \groups -> do
  part1 $ sum $ map anyAnswered groups
  part2 $ sum $ map allAnswered groups

parser :: Parser [Group]
parser = (line `sepBy1` "\n") `sepBy` "\n\n"
  where
    line = Set.fromList <$> many1 letter

anyAnswered :: Group -> Int
anyAnswered = Set.size . Set.unions

allAnswered :: Group -> Int
allAnswered (g :| gs) = Set.size $ foldl' Set.intersection g gs
