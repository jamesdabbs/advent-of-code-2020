module P04 where

import Import hiding (pass, takeWhile)

import           Data.Attoparsec.Text (count, satisfy, takeTill, takeWhile)
import qualified Data.Map             as Map

solve :: Text -> IO ()
solve input = do
  passports <- parse input parser

  let withFields = filter hasRequiredKeys passports
  print $ length withFields -- 242
  print $ length $ filter valid withFields -- 186

valid :: Map Text Text -> Bool
valid = all pass . Map.toList
  where
    pass :: (Text, Text) -> Bool
    pass (key, value) = case Map.lookup key rules of
      Just matcher -> isRight $ parseOnly (matcher <* endOfInput) value
      Nothing -> True

hasRequiredKeys :: Map Text Text -> Bool
hasRequiredKeys m = all
  (flip Map.member m)
  ["byr" , "iyr" , "eyr" , "hgt" , "hcl" , "ecl" , "pid"]

parser :: Parser [Map Text Text]
parser = passport `sepBy` "\n\n"
  where
    passport = fmap Map.fromList $ field `sepBy` (" " <|> "\n")

    field = do
      key <- takeWhile isAlpha
      ":"
      value <- takeTill isSpace
      return (key, value)

rules :: Map Text (Parser ())
rules = Map.fromList
  [ ("byr", range 1920 2002)
  , ("iyr", range 2010 2020)
  , ("eyr", range 2020 2030)
  , ("hgt", void height)
  , ("hcl", void hcl)
  , ("ecl", void $ choice $ map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  , ("pid", void $ pid)
  ]
  where
    range :: Int -> Int -> Parser ()
    range a b = do
      n <- decimal
      guard $ n >= a && n <= b
    height = (range 150 193 *> "cm") <|> (range 59 76 *> "in")
    hcl = "#" *> (count 6 $ satisfy $ inClass "a-z0-9")
    pid = (count 9 $ digit)
