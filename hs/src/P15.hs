module P15 where

import qualified Data.IntMap.Strict as IM
import Import hiding (round)

type Input = [Int]

solution :: Solution Input
solution = solve parser $ \input -> do
  part1 $ spoken input 2020 -- 763
  part2 $ spoken input 30000000 -- 1876406

spoken :: [Int] -> Int -> Int
spoken = go 0 . seed
  where
    seed :: [Int] -> (IM.IntMap Int, Int)
    seed = foldl (\(acc, round) n -> (IM.insert n round acc, round + 1)) (IM.empty, 1)

    go :: Int -> (IM.IntMap Int, Int) -> Int -> Int
    go last (said, round) target
      | round == target = last
      | otherwise =
        go
          (maybe 0 (round -) $ IM.lookup last said)
          (IM.insert last round said, round + 1)
          target

parser :: Parser Input
parser = decimal `sepBy` ","
