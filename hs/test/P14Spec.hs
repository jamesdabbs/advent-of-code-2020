module P14Spec where

import P14
import SpecImport

spec :: Spec
spec = parallel $ do
  it "solves part 1" $ do
    let input =
          parseRight
            parser
            [here|mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0|]

    sum (process v1 input) `shouldBe` 165

  it "solves part 2" $ do
    let input =
          parseRight
            parser
            [here|mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1|]

    sum (process v2 input) `shouldBe` 208
