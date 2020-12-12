module P12 where

import Import hiding (to)

data Heading = N | E | S | W
  deriving (Show, Eq, Enum, Bounded)

data Orientation = R | B | L
  deriving (Show, Eq, Enum, Bounded)

data Instruction = Head Heading Int | Turn Orientation | Forward Int
  deriving (Show, Eq)

type Point = (Int, Int)

type ShipWith a = (Point, a)

type Ship = ShipWith Heading

type Ship' = ShipWith Point

solution :: Solution [Instruction]
solution = solve parser $ \moves -> do
  part1 $ distance $ fst $ foldl follow start moves -- 820
  part2 $ distance $ fst $ foldl follow' start' moves -- 66614

follow :: Ship -> Instruction -> Ship
follow (ship, bearing) (Head direction n) = (translate direction n ship, bearing)
follow (ship, bearing) (Forward n) = (translate bearing n ship, bearing)
follow (ship, bearing) (Turn to) = (ship, turn to bearing)

follow' :: Ship' -> Instruction -> Ship'
follow' (ship, w) (Head direction n) = (ship, translate direction n w)
follow' ((x, y), (wx, wy)) (Forward n) = ((x + n * wx, y + n * wy), (wx, wy))
follow' (ship, waypoint) (Turn to) = (ship, turn' to waypoint)

translate :: Heading -> Int -> Point -> Point
translate N d (x, y) = (x, y + d)
translate E d (x, y) = (x + d, y)
translate S d (x, y) = (x, y - d)
translate W d (x, y) = (x - d, y)

turn :: Orientation -> Heading -> Heading
turn R N = E
turn R E = S
turn R S = W
turn R W = N
turn B N = S
turn B E = W
turn B S = N
turn B W = E
turn L N = W
turn L E = N
turn L S = E
turn L W = S

turn' :: Orientation -> Point -> Point
turn' R (x, y) = (y, - x)
turn' B (x, y) = (- x, - y)
turn' L (x, y) = (- y, x)

start :: ShipWith Heading
start = ((0, 0), E)

start' :: ShipWith Point
start' = ((0, 0), (10, 1))

distance :: Point -> Int
distance (x, y) = abs x + abs y

parser :: Parser [Instruction]
parser = instruction `sepBy` "\n"
  where
    instruction =
      choice
        [ "N" *> (Head N <$> decimal),
          "E" *> (Head E <$> decimal),
          "S" *> (Head S <$> decimal),
          "W" *> (Head W <$> decimal),
          "F" *> (Forward <$> decimal),
          "R90" $> Turn R,
          "R180" $> Turn B,
          "R270" $> Turn L,
          "L90" $> Turn L,
          "L180" $> Turn B,
          "L270" $> Turn R
        ]
