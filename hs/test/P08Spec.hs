module P08Spec where

import P08
import SpecImport
import qualified Data.Map as Map

ex :: Text
ex = [here|nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
|]

spec :: Spec
spec = parallel $ do
  let instructions = parseRight parser ex

  it "parses instructions" $ do
    instructions `shouldBe` Map.fromList
      [ (0, Nop 0),
        (1, Acc 1),
        (2, Jmp 4),
        (3, Acc 3),
        (4, Jmp (-3)),
        (5, Acc (-99)),
        (6, Acc 1),
        (7, Jmp (-4)),
        (8, Acc 6)
        ]

  it "finds the value before looping" $ do
    run instructions `shouldBe` Loop 5

  it "finds a variant that halts" $ do
    findHaltingVariant instructions `shouldBe` Just 8
