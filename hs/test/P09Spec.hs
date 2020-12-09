module P09Spec where

import P09
import SpecImport
import qualified Data.Sequence as Seq

spec :: Spec
spec = parallel $ do
  let input = Seq.fromList
        [ 35
        , 20
        , 15
        , 25
        , 47
        , 40
        , 62
        , 55
        , 65
        , 95
        , 102
        , 117
        , 150
        , 182
        , 127
        , 219
        , 299
        , 277
        , 309
        , 576
        ]

  it "finds the first non-sum of previous" $ do
    scanForNonSum 5 input `shouldBe` Just 127

  it "finds the consecutive sum" $ do
    consecutiveSum 127 input `shouldBe` Just (Seq.fromList [15, 25, 47, 40])
