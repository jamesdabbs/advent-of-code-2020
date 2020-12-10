module P05 where

import Control.Arrow (first)
import Data.Attoparsec.Text (takeWhile)
import qualified Data.Set as Set
import Data.Text (unpack)
import Import hiding (cast, first, takeWhile)

solution :: Solution (Set Int)
solution = solve parser $ \ids -> do
  part1 $ Set.findMax ids -- 864
  part2 $ leastMissing ids -- 739

seatId :: String -> Int
seatId = locate >>> first (* 8) >>> uncurry (+)

-- ex. BFFFBBFRRR ~> (0b1000110, 0b111) ~> (70, 7)
locate :: String -> (Int, Int)
locate = splitAt 7 >>> (cast 'B' *** cast 'R')

cast :: Char -> String -> Int
cast c = foldl' (\acc x -> acc * 2 + if x == c then 1 else 0) 0

leastMissing :: Set Int -> Int
leastMissing s = go (Set.findMin s) s
  where
    go n s' = if (n + 1) `Set.member` s' then go (n + 1) s' else n + 1

parser :: Parser (Set Int)
parser = Set.fromList . map (seatId . unpack) <$> takeWhile (/= '\n') `sepBy` "\n"
