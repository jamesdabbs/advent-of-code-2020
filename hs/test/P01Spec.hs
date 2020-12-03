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
spec = describe "01" $ do
  it "can find pairs" $ do
    findSum 2 input `shouldBe` Just [1721, 299]

  it "can find triples" $ do
    findSum 3 input `shouldBe` Just [979, 366, 675]
