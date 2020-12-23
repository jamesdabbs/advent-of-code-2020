module P23Spec where

import qualified Data.Vector.Unboxed.Mutable as MV
import P23
import SpecImport

spec :: Spec
spec = parallel $ do
  let labels = parseRight parser "389125467"

  describe "initialize" $ do
    it "expands to fit the size if needed" $ do
      cups <- initialize 5 $ [3, 2, 1]
      mapM (MV.read $ successors cups) [0 .. 5] `shouldReturn` [-1, 4, 1, 2, 5, 3]

    it "need not expand" $ do
      cups <- initialize (length labels) labels
      mapM (MV.read $ successors cups) [0 .. 9] `shouldReturn` [-1, 2, 5, 8, 6, 4, 7, 3, 9, 1]

  describe "label" $ do
    it "reads off the sequence of cups from 1" $ do
      cups <- initialize (length labels) labels
      label cups `shouldReturn` "25467389"

  describe "step" $ do
    it "is correct after one step" $ do
      cups <- step =<< initialize (length labels) labels

      label cups `shouldReturn` "54673289"
      current cups `shouldBe` 2

    let examples =
          zip
            [2 ..]
            [ "32546789",
              "34672589",
              "32584679",
              "36792584",
              "93672584",
              "92583674",
              "58392674",
              "83926574",
              "92658374",
              "92637458"
            ]

    forM_ examples $ \(n, expected) ->
      it ("is correct after " <> show n <> " steps") $ do
        cups <- steps n =<< initialize (length labels) labels

        label cups `shouldReturn` expected

  describe "part 1" $ do
    it "solves" $ do
      cups <- steps 100 =<< initialize (length labels) labels
      label cups `shouldReturn` "67384529"
