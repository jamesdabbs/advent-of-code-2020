module P05Spec where

import P05
import SpecImport

import qualified Data.Set as Set

spec :: Spec
spec = parallel $ do
  it "BFFFBBFRRR" $ do
    locate "BFFFBBFRRR" `shouldBe` (70, 7)

  it "FFFBBBFRRR" $ do
    locate "FFFBBBFRRR" `shouldBe` (14, 7)

  it "BBFFBBFRLL" $ do
    locate "BBFFBBFRLL" `shouldBe` (102, 4)

  it "finds the least missing" $
    leastMissing (Set.fromList [7, 9, 3, 4, 5]) `shouldBe` 6
