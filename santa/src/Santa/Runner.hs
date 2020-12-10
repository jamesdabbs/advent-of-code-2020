module Santa.Runner
  ( benchAll,
    fmt,
    run,
  ) where

import Criterion.Types (Config(..))
import Criterion.Main
import qualified Data.Map as Map
import qualified Data.Text as Text
import Protolude
import Text.Printf (printf)
import Santa.Problems (Problems)
import Santa.Solution (Error)
import System.FilePath.Posix ((</>))

benchAll :: Problems -> FilePath -> IO ()
benchAll problems inputs = do
  benches <- mapM mkBench $ Map.toList problems
  defaultMainWith config benches
  putStrLn ("Reports written to ./bench/" :: Text)
  where
    mkBench :: NFData a => (Text, Text -> a) -> IO Benchmark
    mkBench (n, solution) = do
      contents <- input inputs n
      return $ bench (Text.unpack n) $ nf solution contents

    config = defaultConfig
      { reportFile = Just "./bench/report.html"
      , csvFile    = Just "./bench/report.csv"
      , jsonFile   = Just "./bench/report.json"
      }

run :: Problems -> FilePath -> Text -> IO (Either Error (Text, Text))
run problems inputs n = case Map.lookup n problems of
  Just problem -> problem <$> input inputs n
  _ -> die $ "Could not find solution: " <> show n

fmt :: Int -> Text
fmt = Text.pack . printf "%02d"

input :: FilePath -> Text -> IO Text
input inputs n = readFile $ inputs </> Text.unpack n
