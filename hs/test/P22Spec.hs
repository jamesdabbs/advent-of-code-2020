module P22Spec where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import P22
import SpecImport

ex :: Text
ex =
  [here|Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10|]

spec :: Spec
spec = parallel $ do
  let decks = parseRight parser ex

  it "solves part 1" $ do
    combat decks `shouldBe` (Seq.empty, Seq.fromList [3, 2, 10, 6, 8, 5, 9, 4, 7, 1])
    score (snd $ combat decks) `shouldBe` 306

  describe "part 2" $ do
    it "avoids loops" $ do
      let (winner, _, _) = recursiveCombat mempty mempty (Seq.fromList [43, 19], Seq.fromList [2, 29, 14])
      winner `shouldBe` True

    it "solves" $ do
      let (winner, deck, subgames) = recursiveCombat mempty mempty decks
      winner `shouldBe` False

      deck `shouldBe` Seq.fromList [7, 5, 6, 2, 4, 1, 10, 8, 9, 3]
      score deck `shouldBe` 291

      subgames
        `shouldBe` Map.fromList
          [ ((Seq.fromList [9, 8, 5, 2], Seq.fromList [10, 1, 7]), False),
            ((Seq.fromList [8, 1], Seq.fromList [3, 4, 10, 9, 7, 5]), False),
            ((Seq.fromList [8], Seq.fromList [10, 9, 7, 5]), False)
          ]
