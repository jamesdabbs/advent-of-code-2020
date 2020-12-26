module Import
  ( module X,
    countWhere,
    grid,
    inc,
    inspect,
    inspect',
    sepBy1,
    tally,
  )
where

import Control.Arrow as X ((***), (>>>))
import Control.Lens ((+=), (.=), _1, _2)
import Control.Monad.Primitive as X (PrimMonad, PrimState)
import Data.Attoparsec.Text (Parser, atEnd, endOfInput, endOfLine, many', many1', notChar, parseOnly)
import Data.Attoparsec.Text as X (Parser, anyChar, char, choice, decimal, digit, endOfInput, inClass, parseOnly, sepBy, string, takeText)
import Data.Char as X (isAlpha, isSpace)
import Data.Grid as X (Grid)
import qualified Data.Map as Map
import Data.Sequence as X (Seq (..), (<|), (|>))
import Data.String as X (String)
import Protolude as X
import Santa.Solution as X (Solution, part1, part2, solve)

grid :: Parser (Grid Char)
grid = evalStateT cells (0, 0)
  where
    cells = join <$> many' row

    row = do
      _1 .= 0
      tiles <- many1' tile
      lift $ endOfLine <|> void atEnd
      _2 += 1
      return tiles

    tile = do
      c <- lift $ notChar '\n'
      (x, y) <- get
      _1 += 1
      return (x, y, c)

-- A slight variant of the existing `sepBy1` that returns a `NonEmpty` list
sepBy1 :: (Monad f, Alternative f) => f a -> f s -> f (NonEmpty a)
sepBy1 parser delimiter = do
  a <- parser
  as <- many (delimiter *> parser)
  return $ a :| as

inspect :: Show a => a -> a
inspect a = trace (show a :: Text) a

inspect' :: Show a => Text -> a -> a
inspect' label a = trace (label <> ": " <> show a) a

tally :: Ord a => [a] -> Map a Int
tally = foldr inc Map.empty

countWhere :: (a -> Bool) -> [a] -> Int
countWhere f = foldl (\acc a -> if f a then acc + 1 else acc) 0

inc :: Ord a => a -> Map a Int -> Map a Int
inc = Map.alter (Just . maybe 1 (+ 1))
