module P05 where

import qualified Data.Set as Set
import Data.Text (unpack)
import Import hiding (cast)

solve :: Text -> IO ()
solve input = do
  let ids = Set.fromList $ map (seatId . unpack) $ lines input
  print $ Set.findMax ids -- 864
  print $ leastMissing ids -- 739

seatId :: String -> Int
seatId = uncurry ((+) . (8 *)) . locate

-- ex. BFFFBBFRRR ~> (0b1000110, 0b111) ~> (70, 7)
locate :: String -> (Int, Int)
locate s = (cast $ take 7 s, cast $ take 3 $ drop 7 s)

cast :: String -> Int
cast = foldl' (\acc x -> acc * 2 + score x) 0
  where
    score 'B' = 1
    score 'R' = 1
    score _ = 0

leastMissing :: Set Int -> Int
leastMissing s = go (Set.findMin s) s
  where
    go n s' = if (n + 1) `Set.member` s' then go (n + 1) s' else n + 1