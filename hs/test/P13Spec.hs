module P13Spec where

import P13
import SpecImport

ex :: Text
ex =
  [here|939
7,13,x,x,59,x,31,19
|]

spec :: Spec
spec = parallel $ do
  let (ready, buses) = parseRight parser ex

  it "solves part 1" $ do
    earliestDeparture ready (catMaybes buses) `shouldBe` (59, 5)

  describe "part 2" $ do
    it "ex1" $ lineup buses `shouldBe` 1068781
    it "ex2" $ lineup [Just 17, Nothing, Just 13, Just 19] `shouldBe` 3417
    it "ex3" $ lineup [Just 67, Just 7, Just 59, Just 61] `shouldBe` 754018
    it "ex4" $ lineup [Just 67, Nothing, Just 7, Just 59, Just 61] `shouldBe` 779210
    it "ex5" $ lineup [Just 67, Just 7, Nothing, Just 59, Just 61] `shouldBe` 1261476
    it "ex6" $ lineup [Just 1789, Just 37, Just 47, Just 1889] `shouldBe` 1202161486
