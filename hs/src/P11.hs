module P11 where

import qualified Data.Map as Map
import Import hiding (Empty, Location)

data Seat = Empty | Occupied | Floor
  deriving (Show, Eq)

type Location = (Int, Int)

type Vector = (Int, Int)

type Seats = Map Location Seat

type Adjacencies = Map Location [Location]

solution :: Solution Seats
solution = solve parser $ \seats -> do
  part1 $ score $ stabilize (neighborhoods seats) 4 seats -- 2178
  part2 $ score $ stabilize (linesOfSight seats) 5 seats -- 1978

score :: Seats -> Int
score = length . filter (== Occupied) . Map.elems

stabilize :: Adjacencies -> Int -> Seats -> Seats
stabilize adjacencies threshold seats
  | next == seats = seats
  | otherwise = stabilize adjacencies threshold next
  where
    next = evolve adjacencies threshold seats

evolve :: Adjacencies -> Int -> Seats -> Seats
evolve adjacencies threshold seats =
  Map.foldrWithKey
    (\location -> Map.insert location . toggle adjacencies threshold seats location)
    Map.empty
    seats

toggle :: Adjacencies -> Int -> Seats -> Location -> Seat -> Seat
toggle a threshold seats location s
  | s == Empty && adjacent == 0 = Occupied
  | s == Occupied && adjacent >= threshold = Empty
  | otherwise = s
  where
    adjacent =
      Map.findWithDefault [] location a
        & map (`Map.lookup` seats)
        & filter (== Just Occupied)
        & length

neighborhoods :: Seats -> Adjacencies
neighborhoods seats = Map.fromList [(loc, neighbors loc) | loc <- Map.keys seats]
  where
    neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

linesOfSight :: Seats -> Adjacencies
linesOfSight seats = Map.fromList [(loc, visible loc) | loc <- Map.keys seats]
  where
    visible l = mapMaybe (walk l) directions

    walk (x, y) (dx, dy) = case Map.lookup (x + dx, y + dy) seats of
      Just Floor -> walk (x + dx, y + dy) (dx, dy)
      Just _ -> Just (x + dx, y + dy)
      Nothing -> Nothing

directions :: [Vector]
directions = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

parser :: Parser Seats
parser = foldl collect Map.empty <$> grid
  where
    collect s (x, y, c) = case c of
      'L' -> Map.insert (x, y) Empty s
      '#' -> Map.insert (x, y) Occupied s
      '.' -> Map.insert (x, y) Floor s
      _ -> s
