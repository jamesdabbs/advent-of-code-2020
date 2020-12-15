module Santa.Scaffold (scaffold) where

import Protolude
import Data.Text (unpack)
import Santa.Runner (fmt)

scaffold :: FilePath -> Int -> IO ()
scaffold inputs n = do
  writeFile (inputs <> "/" <> show n) ""
  writeFile ("src/P" <> unpack (fmt n) <> ".hs") $ problem $ fmt n
  writeFile ("test/P" <> unpack (fmt n) <> "Spec.hs") $ test $ fmt n

problem :: Text -> Text
problem n = mconcat $ map (<> "\n")
  [ "module P" <> n <> " where"
  , ""
  , "import Import"
  , ""
  , "type Input = [()]"
  , ""
  , "solution :: Solution Input"
  , "solution = solve parser $ \\_ -> do"
  , "  part1 $ ()"
  , "  part2 $ ()"
  , ""
  , "parser :: Parser Input"
  , "parser = line `sepBy` \"\\n\""
  , "  where"
  , "    line = return ()"
  ]

test :: Text -> Text
test n = mconcat $ map (<> "\n")
  [ "module P" <> n <> "Spec where",
    "",
    "import P" <> n,
    "import SpecImport",
    "",
    "ex :: Text",
    "ex =",
    "  [here|",
    "  |]",
    "",
    "spec :: Spec",
    "spec = parallel $ do",
    "  let input = parseRight parser ex",
    "",
    "  it \"solves part 1\" $",
    "    () `shouldBe` ()",
    "",
    "  it \"solves part 2\" $",
    "    () `shouldBe` ()",
    ""
  ]
