module P24Spec where

import qualified Data.Set as Set
import P24
import SpecImport hiding (flip)

ex :: Text
ex =
  [here|sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew|]

spec :: Spec
spec = parallel $ do
  let directions = parseRight parser ex

  describe "locate" $ do
    it "commutes" $
      locate [E, SE, W] `shouldBe` locate [SE]

    it "returns to the origin" $ do
      locate [E, W] `shouldBe` locate []
      locate [NE, SW] `shouldBe` locate []
      locate [SE, NW] `shouldBe` locate []
      locate [NW, W, SW, E, E] `shouldBe` locate []
      locate [NE, E, SE, W, W] `shouldBe` locate []

  it "solves part 1" $
    Set.size (flip directions) `shouldBe` 10

  it "solves part 2" $ do
    Set.size (step $ flip directions) `shouldBe` 15
    Set.size (applyN 100 step $ flip directions) `shouldBe` 2208
