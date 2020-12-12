module P11Spec where

import P11
import SpecImport

ex :: Text
ex = [here|#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
|]

spec :: Spec
spec = parallel $ do
  let seats = parseRight parser ex

  it "solves part 1" $
    score (stabilize (neighborhoods seats) 4 seats) `shouldBe` 37

  it "solves part 2" $
    score (stabilize (linesOfSight seats) 5 seats) `shouldBe` 26
