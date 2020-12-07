module P07Spec where

import P07
import SpecImport
import qualified Data.Set as Set

ex1 :: Text
ex1 = [here|light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
|]

ex2 :: Text
ex2 = [here|shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
|]

spec :: Spec
spec = parallel $ do
  let i1 = parseRight parser ex1
      i2 = parseRight parser ex2

  describe "part 1" $ do
    it "ex1" $
      containers i1 "shiny gold" `shouldBe` Set.fromList ["bright white", "muted yellow", "dark orange", "light red"]

  describe "part 2" $ do
    it "ex1" $ do
      size i1 "shiny gold" `shouldBe` 32

    it "ex2" $ do
      size i2 "shiny gold" `shouldBe` 126
