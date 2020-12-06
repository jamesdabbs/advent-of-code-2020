module Import
  ( module X,
    grid,
    parse,
  )
where

import Control.Arrow as X ((***), (>>>))
import Control.Lens ((+=), (.=), _1, _2)
import Data.Attoparsec.Text (Parser, atEnd, endOfLine, many', many1', notChar, parseOnly)
import Data.Attoparsec.Text as X (Parser, choice, decimal, digit, endOfInput, inClass, parseOnly, sepBy, string)
import Data.Char as X (isAlpha, isSpace)
import Data.String as X (String)
import Data.Text (pack)
import Protolude as X

parse :: Text -> Parser a -> IO a
parse input parser = case parseOnly parser input of
  Left err -> die $ "Failed to parse input: " <> pack err
  Right val -> return val

grid :: Parser [(Int, Int, Char)]
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
