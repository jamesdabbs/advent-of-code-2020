module P17 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Import

type Input = [(Int, Int)]

type P3 = (Int, Int, Int)

type P4 = (Int, Int, Int, Int)

solution :: Solution Input
solution = solve parser $ \input -> do
  part1 $ simulate 6 neighbors3 expand3 input -- 317
  part2 $ simulate 6 neighbors4 expand4 input -- 1692

simulate :: Ord p => Int -> (p -> [p]) -> (a -> Set p) -> a -> Int
simulate ticks neighbors expand = Set.size . applyN ticks (tick neighbors) . expand

tick :: Ord p => (p -> [p]) -> Set p -> Set p
tick neighbors = countNeighbors >>= applyRules
  where
    applyRules neighborCounts actives =
      candidates neighborCounts actives
        & Set.filter (isActive neighborCounts actives)

    isActive neighborCounts actives p =
      case (p `Map.lookup` neighborCounts, p `Set.member` actives) of
        (Just 3, _) -> True
        (Just 2, True) -> True
        _ -> False

    candidates ns ps = Set.fromList (Map.keys ns) <> ps

    countNeighbors = tally . concatMap neighbors

expand3 :: Input -> Set P3
expand3 = Set.fromList . map (\(a, b) -> (a, b, 0))

expand4 :: Input -> Set P4
expand4 = Set.fromList . map (\(a, b) -> (a, b, 0, 0))

neighbors3 :: P3 -> [P3]
neighbors3 (x, y, z) = do
  a <- [x - 1 .. x + 1]
  b <- [y - 1 .. y + 1]
  c <- [z - 1 .. z + 1]
  guard $ (x, y, z) /= (a, b, c)
  return (a, b, c)

neighbors4 :: P4 -> [P4]
neighbors4 (x, y, z, w) = do
  a <- [x - 1 .. x + 1]
  b <- [y - 1 .. y + 1]
  c <- [z - 1 .. z + 1]
  d <- [w - 1 .. w + 1]
  guard $ (x, y, z, w) /= (a, b, c, d)
  return (a, b, c, d)

parser :: Parser Input
parser = gather <$> grid
  where
    gather tuples = [(x, y) | (x, y, c) <- tuples, c == '#']
