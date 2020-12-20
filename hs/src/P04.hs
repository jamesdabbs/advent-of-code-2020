module P04 where

import Data.Attoparsec.Text (count, satisfy, takeTill, takeWhile)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Generic (HKD, enumParser, fields, parseStruct)
import Import hiding (pass, takeWhile)

-- A group of key:value pairs which _may_ represent a valid passport
type FieldSet = Map Text Text

-- We want to store the parsing rules for each Passport field in a record that's
-- structurally similar to the actual Passport record. This will allow us to
-- perform validations in a field-wise generic way.
--
-- See https://reasonablypolymorphic.com/blog/higher-kinded-data for a great
-- writeup of this technique.
data Passport' a = Passport
  { byr :: HKD a Int,
    iyr :: HKD a Int,
    eyr :: HKD a Int,
    hgt :: HKD a Height,
    hcl :: HKD a HexColor,
    ecl :: HKD a EyeColor,
    pid :: HKD a Text,
    cid :: Maybe (HKD a Text)
  }
  deriving (Generic)

data Height = HeightCm Int | HeightIn Int
  deriving (Show, Eq)

newtype HexColor = HexColor Text
  deriving (Show, Eq)

data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
  deriving (Show, Eq, Enum, Bounded)

{- By the above, this is equivalent to

  data Passport = Passport
    { byr :: Int
    , iyr :: Int
    , eyr :: Int
    , hgt :: Height
    , hcl :: HexColor
    , ecl :: EyeColor
    , pid :: Text
    , cid :: Maybe Text
    } deriving Show
-}
type Passport = Passport' Identity

deriving instance Show Passport

deriving instance Eq Passport

solution :: Solution [FieldSet]
solution = solve inputP $ \fieldsets -> do
  part1 $ length $ filter hasRequiredKeys fieldsets -- 242
  part2 $ length $ filter valid fieldsets -- 186

inputP :: Parser [FieldSet]
inputP = passport `sepBy` "\n\n"
  where
    passport = fmap Map.fromList $ field `sepBy` (" " <|> "\n")

    field = do
      key <- takeWhile isAlpha
      ":"
      value <- takeTill isSpace
      return (key, value)

hasRequiredKeys :: FieldSet -> Bool
hasRequiredKeys m = all (`Map.member` m) (fields passportP `Set.difference` Set.singleton "cid")

passportP :: Passport' Parser
passportP =
  Passport
    { byr = rangeP 1920 2002,
      iyr = rangeP 2010 2020,
      eyr = rangeP 2020 2030,
      hgt = heightP,
      hcl = hexColorP,
      ecl = enumParser,
      pid = Text.pack <$> count 9 digit,
      cid = Just takeText
    }

rangeP :: Int -> Int -> Parser Int
rangeP a b = do
  n <- decimal
  guard $ n >= a && n <= b
  return n

heightP :: Parser Height
heightP = centimeters <|> inches
  where
    centimeters = do
      value <- decimal
      "cm"
      guard $ value >= 150 && value <= 193
      return $ HeightCm value
    inches = do
      value <- decimal
      "in"
      guard $ value >= 59 && value <= 76
      return $ HeightCm value

hexColorP :: Parser HexColor
hexColorP = do
  "#"
  digits <- count 6 $ satisfy $ inClass "a-f0-9"
  return $ HexColor $ Text.pack digits

valid :: FieldSet -> Bool
valid = isRight . parseStruct passportP
