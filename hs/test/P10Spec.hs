module P10Spec where

import P10
import SpecImport

a, b :: [Int]
a = 0 : sort
  [ 16
  , 10
  , 15
  , 5
  , 1
  , 11
  , 7
  , 19
  , 6
  , 12
  , 4
  ]

b = 0 : sort
  [ 28
  , 33
  , 18
  , 42
  , 31
  , 14
  , 46
  , 20
  , 48
  , 47
  , 24
  , 23
  , 49
  , 45
  , 19
  , 38
  , 39
  , 11
  , 1
  , 32
  , 25
  , 35
  , 8
  , 17
  , 7
  , 9
  , 4
  , 2
  , 34
  , 10
  , 3
  ]

spec :: Spec
spec = parallel $ do
  describe "part 1" $ do
    it "solves a" $ jolts a `shouldBe` (7, 5)
    it "solves b" $ jolts b `shouldBe` (22, 10)

  describe "part 2" $ do
    it "solves a" $ arrangements a `shouldBe` 8
    it "solves b" $ arrangements b `shouldBe` 19208
