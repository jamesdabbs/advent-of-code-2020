module P17Spec where

import P17
import SpecImport

ex :: Text
ex =
  [here|.#.
..#
###
  |]

spec :: Spec
spec = parallel $ do
  let input = parseRight parser ex

  describe "part 1" $ do
    it "has the right size after 1" $
      simulate 1 neighbors3 expand3 input `shouldBe` 11

    it "has the right size after 2" $
      simulate 2 neighbors3 expand3 input `shouldBe` 21

    it "has the right size after 3" $
      simulate 3 neighbors3 expand3 input `shouldBe` 38

    it "solves" $
      simulate 6 neighbors3 expand3 input `shouldBe` 112

  it "solves part 2" $
    simulate 6 neighbors4 expand4 input `shouldBe` 848
