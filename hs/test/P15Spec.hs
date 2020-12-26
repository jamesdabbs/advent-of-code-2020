module P15Spec where

import P15
import SpecImport hiding (round)

ex :: Text
ex = [here|0,3,6|]

spec :: Spec
spec = parallel $ do
  let input = parseRight parser ex

  let examples =
        [ (7, 1),
          (8, 0),
          (9, 4),
          (2020, 436)
        ]

  forM_ examples $ \(round, expected) ->
    it ("has the expected value for round " <> show round) $
      spoken input round `shouldReturn` expected
