module Main where

import Data.String (String)
import qualified Data.Text as Text
import Protolude
import System.Directory (getDirectoryContents)

{- Scans ./src for filenames matching /(P\d{}).hs/
   Each module should define a `solution :: Solution`
   These will be hoisted to an `IO` action and added to `problems :: Problems`
-}
main :: IO ()
main = do
  _ : _ : outfile : _ <- getArgs
  names <- catMaybes . map problem <$> getDirectoryContents "./src"
  writeFile outfile $ code names

{- Produces a file that looks something like

    module Problems (problems) where

    import $MODULES

    problems :: Problems
    problems = Map.fromList [ (label $MODULE, $MODULE.solution) | $MODULE <- $MODULES ]
-}
code :: [Text] -> Text
code ns = mconcat $ map (<> "\n") $
  [
    "module Problems (problems) where",
    "",
    "import qualified Data.Map as Map"
  ] <> map import' ns <>
  [
    "import Santa (Problems)",
    "import Santa.Solution (run)",
    "",
    "problems :: Problems",
    "problems = Map.fromList"
  ] <> entries ns <> [
    "  ]"
  ]

problem :: String -> Maybe Text
problem ('P' : a : b : ".hs") = Just $ Text.pack [a,b]
problem _ = Nothing

import' :: Text -> Text
import' name = "import qualified P" <> name

entries :: [Text] -> [Text]
entries [] = []
entries (name : names) = entry "[" name : map (entry ",") names
  where
    entry :: Text -> Text -> Text
    entry leader n = mconcat
      [ "  "
      , leader
      , " ( "
      , show n
      , ", run P"
      , n
      , ".solution )"
      ]
