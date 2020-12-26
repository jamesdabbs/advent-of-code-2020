module P15 where

import qualified Data.Vector.Unboxed.Mutable as MV
import Import hiding (round)

type Input = [Int]

solution :: Solution Input
solution = solve parser $ \input -> do
  part1 $ runST $ spoken input 2020 -- 763
  part2 $ runST $ spoken input 30000000 -- 1876406

spoken :: PrimMonad m => [Int] -> Int -> m Int
spoken init len = do
  v <- MV.replicate len unseen
  go v =<< seed v 1 init
  where
    unseen = -1

    seed _ round [last] = return (round, last)
    seed v round (a : as) = do
      MV.write v a round
      seed v (round + 1) as
    seed _ round _ = return (round, unseen)

    go said (round, last)
      | round == len = return last
      | otherwise = do
        previous <- MV.read said last
        MV.write said last round
        go said (round + 1, if previous == unseen then 0 else round - previous)

parser :: Parser Input
parser = decimal `sepBy` ","
