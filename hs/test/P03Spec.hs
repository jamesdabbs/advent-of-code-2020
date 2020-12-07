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
  it "computes the expected path" $ do
    tiles <- parse grid input

    path (wrappedLookup tiles) 3 1 `shouldBe` "####.##.#.."