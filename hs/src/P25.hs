module P25 where

import Import

type Input = (Int, Int)

solution :: Solution Input
solution = solve parser $ \(card, door) -> do
  part1 $ transform (crack 7 card) door -- 20201227

transform :: Int -> Int -> Int
transform loop subject = applyN loop (step subject) 1

step :: Int -> Int -> Int
step subject n = (n * subject) `mod` 20201227

crack :: Int -> Int -> Int
crack subject value = go 0 1
  where
    go loop acc
      | acc == value = loop
      | otherwise = go (loop + 1) (step subject acc)

parser :: Parser Input
parser = (,) <$> decimal <* "\n" <*> decimal
