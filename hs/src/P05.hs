module P05 where

import Import
import Data.Text (unpack)
import qualified Data.Set as Set

solve :: Text -> IO ()
solve input = do
  let ids = Set.fromList $ map (seatId . unpack) $ lines input
  print $ Set.findMax ids -- 864
  print $ leastMissing ids -- 739

seatId :: String -> Int
seatId str =
  let (row, col) = locate str
  in row * 8 + col

locate :: String -> (Int, Int)
locate directions = go directions 0 127 0 7
  where
    go [] row _ col _ = (row, col)
    go (a:as) t b l r = case a of
      'F' -> go as t (avg t b) l r
      'B' -> go as (avg t b) b l r
      'L' -> go as t b l (avg l r)
      'R' -> go as t b (avg l r) r
      _ -> go as t b l r

    avg a b = (a + b + 1) `div` 2

leastMissing :: Set Int -> Int
leastMissing s = go (Set.findMin s) s
  where
    go n s' = if (n + 1) `Set.member` s' then go (n + 1) s' else n + 1