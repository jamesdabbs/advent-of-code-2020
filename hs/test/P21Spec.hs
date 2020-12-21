module P21Spec where

import P21
import SpecImport

ex :: Text
ex =
  [here|mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)|]

spec :: Spec
spec = parallel $ do
  let input = parseRight parser ex
      (ingredients, possibilities) = foldl consider ([], mempty) input
      deductions = deduce possibilities

  it "parses" $
    length input `shouldBe` 4

  it "solves part 1" $
    p1 ingredients deductions `shouldBe` 5

  it "solves part 2" $
    p2 deductions `shouldBe` "mxmxvkd,sqjhc,fvjkl"
