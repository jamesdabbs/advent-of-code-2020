module P03Spec where

import P03
import SpecImport
import Text.Heredoc (str)

input :: Text
input = [str|..##.......
          |#...#...#..
          |.#....#..#.
          |..#.#...#.#
          |.#...##..#.
          |..#.##.....
          |.#.#.#....#
          |.#........#
          |#.##...#...
          |#...##....#
          |.#..#...#.#|]

spec :: Spec
spec = parallel $ do
  let Right tiles = parseOnly grid input

  it "computes the expected path" $ do
    path (wrappedLookup tiles) 3 1 `shouldBe` "####.##.#.."