module P06Spec where

import P06
import SpecImport

ex :: Text
ex = [here|abc

a
b
c

ab
ac

a
a
a
a

b|]

spec :: Spec
spec = parallel $ do
  let input = parseRight parser ex

  it "parses groups" $
    length input `shouldBe` 5

  it "counts items where any answered" $ do
    map anyAnswered input `shouldBe` [3, 3, 3, 1, 1]

  it "counts items where all answer" $ do
    map allAnswered input `shouldBe` [3, 0, 1, 1, 1]
