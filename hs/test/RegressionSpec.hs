module RegressionSpec where

import qualified Data.Text as Text
import Problems (problems)
import Santa.Runner (run)
import SpecImport
import System.Posix.Env (getEnv)

spec :: Spec
spec = do
  shouldRun <- runIO runRegressions

  when shouldRun $
    parallel $
      forM_
        [ ("01", ("73371", "127642310")),
          ("02", ("600", "245")),
          ("03", ("276", "7812180000")),
          ("04", ("242", "186")),
          ("05", ("864", "739")),
          ("06", ("6703", "3430")),
          ("07", ("121", "3805")),
          ("08", ("Loop 1475", "Just 1270")),
          ("09", ("Just 70639851", "Just 8249240")),
          ("10", ("2400", "338510590509056")),
          ("11", ("2178", "1978")),
          ("12", ("820", "66614")),
          ("13", ("5257", "538703333547789")),
          ("14", ("12408060320841", "4466434626828")),
          ("15", ("763", "1876406")),
          ("16", ("30869", "Just 4381476149273")),
          ("17", ("317", "1692")),
          ("18", ("Right 12956356593940", "Right 94240043727614")),
          ("19", ("182", "334")),
          ("20", ("107399567124539", "Just 1555")),
          ("21", ("2317", "\"kbdgs,sqvv,slkfgq,vgnj,brdd,tpd,csfmb,lrnz\"")),
          ("22", ("31754", ""))
        ]
        $ \(n, expected) ->
          it (Text.unpack n) $
            run problems "../inputs" n `shouldReturn` Right expected

runRegressions :: IO Bool
runRegressions = liftA2 (||) (flag "CI") (flag "RUN_REGRESSIONS")
  where
    flag key = do
      found <- getEnv key
      return $ isJust found && found /= Just "false"
