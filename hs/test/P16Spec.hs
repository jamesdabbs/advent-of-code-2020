module P16Spec where

import P16
import SpecImport

ex :: Text
ex =
  [here|class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
|]

spec :: Spec
spec = parallel $ do
  let Input {..} = parseRight parser ex
      (validTickets, errors) = validate rules others

  describe "part 1" $ do
    it "solves" $
      errors `shouldBe` [4, 55, 12]

  describe "part 2" $ do
    it "solves" $
      greedyPick (ruleOut rules validTickets) `shouldBe` Just ["row", "class", "seat"]

  describe "greedyPick" $ do
    it "can pick ints" $
      greedyPick ([[1, 2, 3, 4, 5], [1, 2, 3, 4], [2], [1, 2], [1, 2, 3]] :: [[Int]]) `shouldBe` Just [5, 4, 2, 1, 3]

    it "can fail to find a solution" $
      greedyPick ([[1], [1], [1, 2, 3]] :: [[Int]]) `shouldBe` Nothing

  describe "score" $ do
    it "multiplies columns with a prefix" $
      score "b" [1, 2, 3, 4] ["a", "b", "b", "a"] `shouldBe` 6
