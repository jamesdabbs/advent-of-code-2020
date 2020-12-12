module P12Spec where

import P12
import SpecImport

ex :: Text
ex =
  [here|F10
N3
F7
R90
F11|]

spec :: Spec
spec = parallel $ do
  let moves = parseRight parser ex

  it "solves part 1" $
    foldl follow start moves `shouldBe` ((17, -8), S)

  it "solves part 2" $
    foldl follow' start' moves `shouldBe` ((214, -72), (4, -10))
