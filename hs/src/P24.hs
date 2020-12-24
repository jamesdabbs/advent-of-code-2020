module P24 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Import hiding (flip)

data Direction = E | SE | SW | W | NW | NE
  deriving (Show, Eq)

type Point = (Int, Int, Int)

type Input = [[Direction]]

solution :: Solution Input
solution = solve parser $ \directions -> do
  let tiles = flip directions
  part1 $ Set.size tiles
  part2 $ Set.size $ applyN 100 step tiles

locate :: [Direction] -> Point
locate = foldl move (0, 0, 0)

move :: Point -> Direction -> Point
move (a, b, c) NE = (a + 1, b - 1, c)
move (a, b, c) E = (a + 1, b, c - 1)
move (a, b, c) SE = (a, b + 1, c - 1)
move (a, b, c) SW = (a - 1, b + 1, c)
move (a, b, c) W = (a - 1, b, c + 1)
move (a, b, c) NW = (a, b - 1, c + 1)

flip :: [[Direction]] -> Set Point
flip = foldl (\acc directions -> toggle (locate directions) acc) Set.empty

toggle :: Ord a => a -> Set a -> Set a
toggle a as =
  if Set.member a as
    then Set.delete a as
    else Set.insert a as

step :: Set Point -> Set Point
step ps = Set.filter active candidates
  where
    neighbors :: Map Point Int
    neighbors = foldl (\acc -> foldr inc acc . neighborhood) Map.empty ps

    candidates = ps `Set.union` Set.fromList (Map.keys neighbors)

    active :: Point -> Bool
    active p = case (Set.member p ps, Map.findWithDefault 0 p neighbors) of
      (_, 2) -> True
      (True, 1) -> True
      _ -> False

neighborhood :: Point -> [Point]
neighborhood p = map (move p) [E, SE, SW, W, NW, NE]

parser :: Parser Input
parser = steps `sepBy` "\n"
  where
    steps =
      some $
        choice
          [ SE <$ "se",
            SW <$ "sw",
            E <$ "e",
            NE <$ "ne",
            NW <$ "nw",
            W <$ "w"
          ]
