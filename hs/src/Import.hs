module Import
  ( module X,
    grid,
    inspect,
    inspect',
    sepBy1,
  )
where

import Control.Arrow as X ((***), (>>>))
import Control.Lens ((+=), (.=), _1, _2)
import Data.Attoparsec.Text (Parser, atEnd, endOfInput, endOfLine, many', many1', notChar, parseOnly)
import Data.Attoparsec.Text as X (Parser, choice, decimal, digit, endOfInput, inClass, parseOnly, sepBy, string)
import Data.Char as X (isAlpha, isSpace)
import Data.Grid as X (Grid)
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