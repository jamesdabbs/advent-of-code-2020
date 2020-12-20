module P20 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Import hiding (flip, rotate)

type Input = [Tile]

type Point = (Int, Int)

type Signature = String

data Tile = Tile
  { label :: Int,
    points :: Set Point
  }
  deriving (Show, Eq, Ord)

type Tiles = Map Signature [Tile]

solution :: Solution Input
solution = solve parser $ \input -> do
  let tiles = index input
  case cornersByHash tiles of
    Left err -> part1 err
    Right corners -> do
      part1 $ product $ map label corners -- 107399567124539
      part2 $ fmap (roughness . coalesce) $ arrange tiles =<< head corners -- Just 1555

index :: [Tile] -> Tiles
index = foldl indexBySignatures Map.empty
  where
    signatures tile = boundary tile <> map reverse (boundary tile)
    indexBySignatures acc t = foldl (\m s -> push s t m) acc $ signatures t

hashCounts :: Tiles -> [(Tile, [Int])]
hashCounts tiles =
  let lookup k = length $ Map.findWithDefault [] k tiles
      count t = (t, map lookup $ boundary t)
   in map count $ ordNub $ join $ Map.elems tiles

cornerCandidates :: Tiles -> [(Tile, [Int])]
cornerCandidates = filter (candidate . snd) . hashCounts
  where
    candidate = (== 2) . length . filter (== 2)

cornersByHash :: Tiles -> Either Text [Tile]
cornersByHash tiles =
  let candidates = map fst $ cornerCandidates tiles
   in if length candidates == 4
        then Right candidates
        else Left $ "Found " <> show (length candidates) <> " candidates"

inTopLeft :: Tiles -> Tile -> Maybe Tile
inTopLeft tiles tile = find (\o -> count (top o) == 1 && count (left o) == 1) $ orientations tile
  where
    count b = length $ Map.findWithDefault [] b tiles

arrangeRow :: Tiles -> Tile -> [Tile]
arrangeRow tiles tile = go [tile] tile
  where
    go acc curr = case glue right left curr tiles of
      Just next -> go (acc <> [next]) next
      _ -> acc

arrange :: Tiles -> Tile -> Maybe [[Tile]]
arrange tiles tile = go [] . arrangeRow tiles <$> inTopLeft tiles tile
  where
    go acc (a : as) = case glue bottom top a tiles of
      Just b -> go (acc <> [a : as]) $ arrangeRow tiles b
      Nothing -> acc <> [a : as]
    go acc [] = acc

coalesce :: [[Tile]] -> Set Point
coalesce = foldl addRow Set.empty . zip [0 ..]
  where
    addRow :: Set Point -> (Int, [Tile]) -> Set Point
    addRow points (row, tiles) = foldl (\acc (col, tile) -> acc `Set.union` embedAt row col tile) points $ zip [0 ..] tiles

embedAt :: Int -> Int -> Tile -> Set Point
embedAt row col = foldl add Set.empty . points
  where
    add :: Set Point -> Point -> Set Point
    add acc (x, y)
      | x == 0 || y == 0 || x == width - 1 || y == width - 1 = acc
      | otherwise = (col * (width - 2) + x - 1, row * (width - 2) + y - 1) `Set.insert` acc

findMonsters :: Set Point -> [[Point]]
findMonsters ps =
  maybe [] identity $
    find (not . null) $
      map (\template -> mapMaybe (sightMonsterAt template ps) $ toList ps) monsters

sightMonsterAt :: [Point] -> Set Point -> Point -> Maybe [Point]
sightMonsterAt template ps (x, y) = if all (`Set.member` ps) locations then Just locations else Nothing
  where
    locations = map (\(a, b) -> (x + a, y + b)) template

monster :: [Point]
monster = [(0, 0), (1, 1), (4, 1), (5, 0), (6, 0), (7, 1), (10, 1), (11, 0), (12, 0), (13, 1), (16, 1), (17, 0), (18, 0), (18, -1), (19, 0)]

monsters :: [[Point]]
monsters =
  [ monster,
    map (\(x, y) -> (x, - y)) monster,
    map (\(x, y) -> (- x, y)) monster,
    map (\(x, y) -> (- x, - y)) monster,
    map (\(x, y) -> (y, x)) monster,
    map (\(x, y) -> (y, - x)) monster,
    map (\(x, y) -> (- y, x)) monster,
    map (\(x, y) -> (- y, - x)) monster
  ]

roughness :: Set Point -> Int
roughness ps = Set.size $ ps `Set.difference` Set.fromList (join $ findMonsters ps)

glue :: (Tile -> Signature) -> (Tile -> Signature) -> Tile -> Tiles -> Maybe Tile
glue dir rev t tiles =
  let sig = dir t
   in orient rev sig =<< find (\a -> label a /= label t) (Map.findWithDefault [] sig tiles)

orient :: (Tile -> Signature) -> Signature -> Tile -> Maybe Tile
orient dir s t = find (\o -> dir o == s) $ orientations t

rotate :: Tile -> Tile
rotate (Tile lbl points) = Tile lbl $ Set.map (\(x, y) -> (y, width - 1 - x)) points

flip :: Tile -> Tile
flip (Tile lbl points) = Tile lbl $ Set.map (\(x, y) -> (x, width - 1 - y)) points

rotations :: Tile -> [Tile]
rotations t = [t, rotate t, rotate (rotate t), rotate (rotate (rotate t))]

orientations :: Tile -> [Tile]
orientations t = concatMap rotations [t, flip t]

parser :: Parser Input
parser = tile `sepBy` "\n"
  where
    tile = buildTile <$> ("Tile " *> decimal <* ":\n") <*> grid

buildTile :: Int -> [(Int, Int, Char)] -> Tile
buildTile label cells =
  Tile
    { label,
      points = Set.fromList [(x, y) | (x, y, c) <- cells, c == '#']
    }

boundary :: Tile -> [String]
boundary tile = [top tile, right tile, bottom tile, left tile]

scan :: [Point] -> Tile -> Signature
scan edge (Tile _ points) = map (\p -> if Set.member p points then '.' else '_') edge

top, left, bottom, right :: Tile -> Signature
top = scan topE
left = scan leftE
bottom = scan bottomE
right = scan rightE

width :: Int
width = 10

topE, leftE, bottomE, rightE :: [Point]
topE = [(x, 0) | x <- [0 .. width - 1]]
leftE = [(0, y) | y <- [0 .. width - 1]]
bottomE = [(x, width - 1) | x <- [0 .. width - 1]]
rightE = [(width - 1, y) | y <- [0 .. width - 1]]

push :: Ord k => k -> v -> Map k [v] -> Map k [v]
push k v = Map.alter (Just . maybe [v] (v :)) k
