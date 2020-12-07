module P02 where

import Data.Attoparsec.Text (letter, takeTill)
import qualified Data.Text as Text
import Import hiding (chr, max, min)

data Policy = Policy
  { min :: Int,
    max :: Int,
    chr :: Char
  }
  deriving (Show, Eq)

type Password = Text

type Strategy = Policy -> Password -> Bool

solve :: Text -> IO ()
solve input = do
  entries <- parse parser input

  print $ length $ filter (uncurry valid) entries -- 600
  print $ length $ filter (uncurry revalid) entries -- 245

parser :: Parser [(Policy, Password)]
parser = line `sepBy` "\n"
  where
    line = do
      min <- decimal
      "-"
      max <- decimal
      " "
      chr <- letter
      ": "
      password <- takeTill (== '\n')
      return
        ( Policy {min, max, chr},
          password
        )

valid :: Strategy
valid Policy {min, max, chr} password = count >= min && count <= max
  where
    count = Text.foldl (\n c -> if c == chr then n + 1 else n) 0 password

revalid :: Strategy
revalid Policy {min, max, chr} password = (at min == chr) /= (at max == chr)
  where
    at :: Int -> Char
    at pos = maybe '\0' fst $ Text.uncons $ Text.drop (pos - 1) password
