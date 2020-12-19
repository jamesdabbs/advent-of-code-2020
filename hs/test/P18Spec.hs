module P18Spec where

import qualified Data.Text as Text
import P18
import SpecImport hiding (evaluate, exp, group)

spec :: Spec
spec = parallel $ do
  describe "part1" $ do
    let strategy = groupsIn associate

    describe "syntax errors" $ do
      it "handles duplicate terms without an operator" $
        strategy (toExp "1 2 3 + 4 + 5") `shouldBe` Left (SyntaxError "walk failed at (1 2 3)")

      it "handles operators without terms" $
        strategy (toExp "*") `shouldBe` Left (SyntaxError "walk failed at (*)")

    forM_
      [ ("1 + 2 * 3 + 4 * 5 + 6", 71),
        ("2 * 3 + (4 * 5)", 26),
        ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
        ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
        ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632)
      ]
      $ \(exp, val) ->
        it (Text.unpack $ "computes " <> exp) $
          fmap evaluate (strategy $ toExp exp) `shouldBe` Right val

  describe "part2" $ do
    let strategy = groupsIn assocPlus

    it "groups additions together" $
      renderAST
        <$> strategy
          ( toExp "1 + 2 + 3 * 4 + 5 + 6 * 7 + 8 + 9"
          )
        `shouldBe` Right "((7 + 8 + 9) * ((4 + 5 + 6) * (1 + 2 + 3)))"

    forM_
      [ ("1 + 2 * 3 + 4 * 5 + 6", 231),
        ("1 + (2 * 3) + (4 * (5 + 6))", 51),
        ("2 * 3 + (4 * 5)", 46),
        ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445),
        ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060),
        ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340)
      ]
      $ \(exp, val) ->
        it (Text.unpack $ "computes " <> exp) $
          fmap evaluate (strategy $ toExp exp) `shouldBe` Right val

toExp :: Text -> Expression
toExp = parseRight parser