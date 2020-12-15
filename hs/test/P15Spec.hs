module P15Spec where

import P15
import SpecImport

ex :: Text
ex = [here|0,3,6|]

spec :: Spec
spec = parallel $ do
  let input = parseRight parser ex

  it "solves part 1" $ do
    spoken input 4 `shouldBe` 0
    spoken input 5 `shouldBe` 3
    spoken input 6 `shouldBe` 3
    spoken input 7 `shouldBe` 1
    spoken input 8 `shouldBe` 0
    spoken input 9 `shouldBe` 4
    spoken input 2020 `shouldBe` 436
