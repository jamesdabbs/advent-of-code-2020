module P15 where

import qualified Data.Map as Map
import Import hiding (round)

type Input = [Int]

solution :: Solution Input
solution = solve parser $ \input -> do
  part1 $ spoken input 2020 -- 763
  part2 $ spoken input 30000000 -- 1876406

spoken :: [Int] -> Int -> Int
spoken = go 0 . seed
  where
    seed :: [Int] -> (Map Int Int, Int)
    seed = foldl (\(acc, round) n -> (Map.insert n round acc, round + 1)) (Map.empty, 1)

    go :: Int -> (Map Int Int, Int) -> Int -> Int
    go last (said, round) target
      | round == target = last
      | otherwise =
        go
          (maybe 0 (round -) $ Map.lookup last said)
          (Map.insert last round said, round + 1)
          target

parser :: Parser Input
parser = decimal `sepBy` ","
