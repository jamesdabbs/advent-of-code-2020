module P01Spec where

import Import
import P01
import Test.Hspec

input :: [Int]
input =
  [ 1721,
    979,
    366,
    299,
    675,
    1456
  ]

spec :: Spec
spec = parallel $ do
  it "can find pairs" $ do
    findPair 2020 input `shouldBe` Just [299, 1721]

  it "can find triples" $ do
    findTriple 2020 input `shouldBe` Just [979, 675, 366]
