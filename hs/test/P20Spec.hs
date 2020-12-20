{-# OPTIONS_GHC -fno-warn-orphans #-}

module P20Spec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import P20
import SpecImport hiding (flip, rotate)
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Prelude (zip3)

spec :: Spec
spec = parallel $ do
  let input = parseRight parser ex
      tiles = index input
      Just corner = find (\t -> label t == 1951) input

  it "can generate orientations" $ do
    length (ordNub $ orientations $ tileWith [(0, 0), (1, 0), (1, 1)]) `shouldBe` 8

  describe "rotate" $ do
    it "rotates once" $
      rotate (tileWith [(0, 0), (1, 0)]) `shouldBe` tileWith [(0, 9), (0, 8)]

    it "rotates twice" $
      applyN 2 rotate (tileWith [(0, 0), (1, 0)]) `shouldBe` tileWith [(8, 9), (9, 9)]

    prop "four rotations is the identity" $ \x -> applyN 4 rotate x == x

  describe "flip" $ do
    it "flips once" $
      flip (tileWith [(0, 0), (1, 0), (1, 1)]) `shouldBe` tileWith [(0, 9), (1, 9), (1, 8)]

    prop "two flips is the " $ \x -> applyN 2 flip x == x

  describe "boundary" $ do
    let [a, b, c, d] = boundary corner

    it "computes the boundary" $
      [a, b, c, d] `shouldBe` ["._..___.._", "_.....__._", ".___.._.__", ".._.__.__."]

    it "rotates the boundary" $
      boundary (rotate corner) `shouldBe` [b, reverse c, d, reverse a]

    it "flips the boundary" $
      boundary (flip corner) `shouldBe` [c, reverse b, a, reverse d]

  describe "part 1" $ do
    it "parses" $
      length input `shouldBe` 9

    it "finds the correct corners" $
      fmap (sort . map label) (cornersByHash tiles) `shouldBe` Right [1171, 1951, 2971, 3079]

  describe "part 2" $ do
    let Just topLeft = inTopLeft tiles corner
        Just arranged = arrange tiles topLeft
        coalesced = coalesce arranged

    it "can arrange the top row" $ do
      let topRow = arrangeRow tiles topLeft
      map label topRow `shouldBe` [1951, 2729, 2971]

    it "can find the row below the top row" $ do
      let Just below = glue bottom top topLeft tiles
      map label (arrangeRow tiles below) `shouldBe` [2311, 1427, 1489]

    it "arranges the whole grid" $
      map (map label) arranged
        `shouldBe` transpose
          [ [1951, 2311, 3079],
            [2729, 1427, 2473],
            [2971, 1489, 1171]
          ]

    describe "render" $ do
      let rendered = map Text.unpack . lines $ render coalesced
          expected =
            transpose $
              map Text.unpack $
                lines
                  [here|.#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###|]

      forM_ (zip3 [0 ..] rendered expected) $ \(n, r, e) ->
        it ("renders row " <> show (n :: Int)) $ r `shouldBe` e

    describe "monsters" $ do
      it "finds monsters" $
        map head (findMonsters coalesced) `shouldBe` [Just (1, 17), Just (2, 3)]

      it "finds roughness" $
        roughness coalesced `shouldBe` 273

instance Arbitrary Tile where
  arbitrary = Tile <$> arbitrary <*> arbitrary

tileWith :: [Point] -> Tile
tileWith = Tile 1 . Set.fromList

render :: Set Point -> Text
render ps = unlines $ map row [0 .. ymax]
  where
    xmax = maximum $ map fst $ toList ps
    ymax = maximum $ map snd $ toList ps
    row y = Text.pack $ map (\x -> if Set.member (x, y) ps then '#' else '.') [0 .. xmax]

ex :: Text
ex =
  [here|Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...|]
