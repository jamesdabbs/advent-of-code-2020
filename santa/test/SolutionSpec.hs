module SolutionSpec where

import Santa.Solution
import SpecImport
import Data.Attoparsec.Text (decimal, sepBy)

solution :: Solution [Int]
solution = solve (decimal `sepBy` "\n") $ \ns -> do
  part1 $ sum ns
  part2 $ product ns

spec :: Spec
spec = do
  it "can run" $ do
    run solution "2\n3\n4" `shouldBe` Right ("9", "24")
