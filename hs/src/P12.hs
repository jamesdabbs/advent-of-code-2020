module P12 where

import Import hiding (rotate)

data Direction = N | E | S | W | L | R | F
  deriving (Show, Eq, Enum, Bounded)

data Heading = North | East | South | West
  deriving (Show, Eq, Enum, Bounded)

type Instruction = (Direction, Int)

type Ship = (Int, Int, Heading)

type Ship' = ((Int, Int), (Int, Int))

solution :: Solution [Instruction]
solution = solve parser $ \moves -> do
  part1 $ distance $ location $ foldl follow start moves -- 820
  part2 $ distance $ fst $ foldl follow' start' moves -- 66614

follow :: Ship -> Instruction -> Ship
follow (x, y, h) (N, d) = (x, y + d, h)
follow (x, y, h) (E, d) = (x + d, y, h)
follow (x, y, h) (S, d) = (x, y - d, h)
follow (x, y, h) (W, d) = (x - d, y, h)
-- Forward
follow (x, y, North) (F, d) = (x, y + d, North)
follow (x, y, East) (F, d) = (x + d, y, East)
follow (x, y, South) (F, d) = (x, y - d, South)
follow (x, y, West) (F, d) = (x - d, y, West)
-- Turning
follow (x, y, h) (R, d) = (x, y, applyN (d `div` 90) rotate h)
follow s (L, d) = follow s (R, 360 - d)

follow' :: Ship' -> Instruction -> Ship'
follow' (p, (wx, wy)) (N, d) = (p, (wx, wy + d))
follow' (p, (wx, wy)) (E, d) = (p, (wx + d, wy))
follow' (p, (wx, wy)) (S, d) = (p, (wx, wy - d))
follow' (p, (wx, wy)) (W, d) = (p, (wx - d, wy))
-- Forward
follow' ((x, y), (wx, wy)) (F, d) = ((x + d * wx, y + d * wy), (wx, wy))
-- Turning
follow' (x, w) (R, d) = (x, rotate' d w)
follow' s (L, d) = follow' s (R, 360 - d)

start :: Ship
start = (0, 0, East)

start' :: Ship'
start' = ((0, 0), (10, 1))

rotate :: Heading -> Heading
rotate h
  | h == maxBound = minBound
  | otherwise = succ h

rotate' :: Int -> (Int, Int) -> (Int, Int)
rotate' 90 (x, y) = (y, - x)
rotate' 180 (x, y) = (- x, - y)
rotate' 270 (x, y) = (- y, x)
rotate' _ w = w

location :: Ship -> (Int, Int)
location (x, y, _) = (x, y)

distance :: (Int, Int) -> Int
distance (x, y) = abs x + abs y

parser :: Parser [Instruction]
parser = ((,) <$> directions <*> decimal) `sepBy` "\n"
  where
    directions = choice $ map direction [minBound .. maxBound]
    direction d = string (show d) $> d
