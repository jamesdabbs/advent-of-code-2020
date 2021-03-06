module P10 where

import qualified Data.Map.Strict as Map
import Import hiding (get)

solution :: Solution [Int]
solution = solve parser $ \numbers -> do
  part1 $ uncurry (*) $ jolts numbers -- 2400
  part2 $ arrangements numbers -- 338510590509056

jolts :: [Int] -> (Int, Int)
jolts adapters = (get 1 counts, get 3 counts + 1)
  where
    counts = tally $ differences adapters

differences :: [Int] -> [Int]
differences (a : b : cs) = b - a : differences (b : cs)
differences _ = []

{- The idea here is to walk from the end backwards, memoizing for each n the
   number of paths which start at n.

   Assumes that [Int] is sorted, both s.t. the sweep always finds a memoized
   value, and s.t. the somewhat-more-efficient implementation of `nexts` is
   correct.
-}
arrangements :: [Int] -> Integer
arrangements l = get 0 $ foldr expand (Map.singleton (maximum l) 1) l
  where
    nexts a = takeWhile (<= a + 3) $ dropWhile (<= a) l

    expand n m =
      if n `Map.member` m
        then m
        else Map.insert n (sum $ map (m Map.!) $ nexts n) m

parser :: Parser [Int]
parser = (0 :) . sort <$> decimal `sepBy` "\n"

get :: (Ord a, Integral v) => a -> Map a v -> v
get = Map.findWithDefault 0
