module P01 where

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Import

solution :: Solution [Int]
solution = solve (decimal `sepBy` "\n") $ \numbers -> do
  part1 $ product $ fromJust $ findPair 2020 numbers -- 73371
  part2 $ product $ fromJust $ findTriple 2020 numbers -- 127642310

findPair :: Int -> [Int] -> Maybe [Int]
findPair target = go Set.empty
  where
    go seen (a : as) =
      if (target - a) `Set.member` seen
        then Just [a, target - a]
        else go (a `Set.insert` seen) as
    go _ _ = Nothing

findTriple :: Int -> [Int] -> Maybe [Int]
findTriple target (a : as) = case findPair (target - a) as of
  Just ns -> Just (a : ns)
  _ -> findTriple target as
findTriple _ _ = Nothing
