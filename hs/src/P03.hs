module P03 where

import qualified Data.Map as Map
import Import

solve :: Text -> IO ()
solve input = do
  tiles <- parse grid input

  let atlas = wrappedLookup tiles
      grade (over, down) = length $ filter (== '#') $ path atlas over down

  print $ grade (3, 1) -- 276
  print $ product $ map grade [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] -- 7812180000

wrappedLookup :: [(Int, Int, a)] -> Int -> Int -> Maybe a
wrappedLookup tiles x y = Map.lookup (x `mod` xmax, y) index
  where
    index = Map.fromList [((a, b), c) | (a, b, c) <- tiles]
    xmax = maximum [a | (a, _, _) <- tiles] + 1

path :: (Int -> Int -> Maybe a) -> Int -> Int -> [a]
path atlas over down = loop 0 0 []
  where
    loop x y acc = case atlas x y of
      Just a -> loop (x + over) (y + down) (a : acc)
      _ -> acc