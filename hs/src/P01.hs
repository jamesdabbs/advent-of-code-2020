module P01 where

import Data.Maybe (fromJust)
import Import

solution :: Solution [Int]
solution = solve (decimal `sepBy` "\n") $ \numbers -> do
  part1 $ product $ fromJust $ findSum 2 numbers -- 73371
  part2 $ product $ fromJust $ findSum 3 numbers -- 127642310

findSum :: Int -> [Int] -> Maybe [Int]
findSum ofSize = find ((== 2020) . sum) . tuples ofSize

tuples :: Int -> [a] -> [[a]]
tuples _ [] = []
tuples 0 _ = [[]]
tuples n (a : as) = map (a :) (tuples (n - 1) as) ++ tuples n as
