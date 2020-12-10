module Santa.Main where

import Options.Applicative
import Protolude
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.Environment (withArgs)

import Santa.Problems (Problems)
import qualified Santa.Runner as Santa

data Command
  = List
  | Run (Maybe Int)
  | Bench (Maybe Int)

defaultMain :: Problems -> FilePath -> IO ()
defaultMain problems inputs = execParser (info (opts <**> helper) idm) >>= exec problems inputs

exec :: Problems -> FilePath -> Command -> IO ()
exec problems inputs (Run (Just n)) = run problems inputs $ Santa.fmt n
exec problems inputs (Run _) = runAll problems inputs

-- KLUDGE: we're currently using criterion's defaultMain, which parses args itself,
-- so we fake them with withArgs
exec problems inputs (Bench (Just n)) = withArgs [Text.unpack $ Santa.fmt n] $ Santa.benchAll problems inputs
exec problems inputs (Bench _) = withArgs [] $ Santa.benchAll problems inputs

exec problems _ List = putStrLn $ "Solutions available: " <> Text.intercalate "," (map show $ Map.keys problems)

opts :: Parser Command
opts =
  subparser
    ( command "run" (info runP (progDesc "Run a solution"))
        <> command "list" (info (pure List) (progDesc "List available solutions"))
        <> command "bench" (info benchP (progDesc "Benchmark solutions"))
    )
  where
    runP = Run <$> optional (argument auto (metavar "n"))
    benchP = Bench <$> optional (argument auto (metavar "n"))

run :: Problems -> FilePath -> Text -> IO ()
run problems inputs n = Santa.run problems inputs n >>= \case
  Left err -> putStrLn ("Error: " <> show err :: Text)
  Right (a, b) -> putStrLn a >> putStrLn b

runAll :: Problems -> FilePath -> IO ()
runAll problems inputs = forM_ (Map.keys problems) $ \n -> do
  putStrLn $ "==> " <> n
  run problems inputs n
