module P04 where

import Import hiding (pass, takeWhile)

import           Data.Attoparsec.Text (count, satisfy, takeTill, takeWhile)
import qualified Data.Map             as Map
import qualified Data.Text            as Text

-- A group of key:value pairs which _may_ represent a valid passport
type FieldSet = Map Text Text

-- We want to store the parsing rules for each Passport field in a record that's
-- structurally similar to the actual Passport record. This will allow us to
-- perform validations in a field-wise generic way.
--
-- See https://reasonablypolymorphic.com/blog/higher-kinded-data for a great
-- writeup of this technique.
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data Passport' a = Passport
  { byr :: HKD a Int
  , iyr :: HKD a Int
  , eyr :: HKD a Int
  , hgt :: HKD a Height
  , hcl :: HKD a HexColor
  , ecl :: HKD a EyeColor
  , pid :: HKD a Text
  } deriving Generic

data Height = HeightCm Int | HeightIn Int
  deriving (Show, Eq)

newtype HexColor = HexColor Text
  deriving (Show, Eq)

data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
  deriving (Show, Eq)

{- By the above, this is equivalent to

  data Passport = Passport
    { byr :: Int
    , iyr :: Int
    , eyr :: Int
    , hgt :: Height
    , hcl :: HexColor
    , ecl :: EyeColor
    , pid :: Text
    } deriving Show
-}
type Passport = Passport' Identity
deriving instance Show Passport

solve :: Text -> IO ()
solve input = do
  fieldsets <- parse input inputP

  print $ length $ filter hasRequiredKeys fieldsets -- 242
  print $ length $ filter valid fieldsets -- 186

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
hasRequiredKeys m = all
  (flip Map.member m)
  ["byr" , "iyr" , "eyr" , "hgt" , "hcl" , "ecl" , "pid"]

passportP :: Passport' Parser
passportP = Passport
  { byr = rangeP 1920 2002
  , iyr = rangeP 2010 2020
  , eyr = rangeP 2020 2030
  , hgt = heightP
  , hcl = hexColorP
  , ecl = eyeColorP
  , pid = Text.pack <$> count 9 digit
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
  digits <- count 6 $ satisfy $ inClass "a-z0-9"
  return $ HexColor $ Text.pack digits

eyeColorP :: Parser EyeColor
eyeColorP = choice $ map test
  [ ("amb", Amb)
  , ("blu", Blu)
  , ("brn", Brn)
  , ("gry", Gry)
  , ("grn", Grn)
  , ("hzl", Hzl)
  , ("oth", Oth)
  ]
  where
    test :: (Text, EyeColor) -> Parser EyeColor
    test (key, color) = string key *> return color

-- TODO: similar to https://reasonablypolymorphic.com/blog/higher-kinded-data
-- we should be able to define a
--
--   parseFields :: f Parser -> FieldSet -> Maybe (f Identity)
--
-- for a suitably generic f, which we can then specialize to f = Passport' and
-- get this for free.
parseFields :: Passport' Parser -> FieldSet -> Maybe Passport
parseFields Passport {..} fields = Passport
  <$> parseField "byr" byr
  <*> parseField "iyr" iyr
  <*> parseField "eyr" eyr
  <*> parseField "hgt" hgt
  <*> parseField "hcl" hcl
  <*> parseField "ecl" ecl
  <*> parseField "pid" pid
  where
    parseField :: Text -> Parser a -> Maybe a
    parseField key parser = case Map.lookup key fields of
      Just value -> case parseOnly (parser <* endOfInput) value of
        Right parsed -> return parsed
        _ -> Nothing
      _ -> Nothing

valid :: FieldSet -> Bool
valid = isJust . parseFields passportP
