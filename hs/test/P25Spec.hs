module P25Spec where

import P25
import SpecImport

spec :: Spec
spec = parallel $ do
  describe "transform" $ do
    it "ex1" $
      transform 8 7 `shouldBe` 5764801

    it "ex2" $
      transform 11 7 `shouldBe` 17807724

  describe "crack" $ do
    it "ex1" $
      crack 7 5764801 `shouldBe` 8

    it "ex2" $
      crack 7 17807724 `shouldBe` 11
