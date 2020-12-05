module P02Spec where

import Import hiding (chr)
import P02
import Test.Hspec

input :: [(Policy, Password)]
input =
  [ (Policy 1 3 'a', "abcde"),
    (Policy 1 3 'b', "cdefg"),
    (Policy 2 9 'c', "ccccccccc")
  ]

matches :: Strategy -> [(Policy, Password)] -> [Char]
matches strategy = map (chr . fst) . filter (uncurry strategy)

spec :: Spec
spec = parallel $ do
  it "can apply the first strategy" $ do
    matches valid input `shouldBe` "ac"

  it "can apply the second strategy" $ do
    matches revalid input `shouldBe` "a"
