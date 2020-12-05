module GenericSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Generic
import SpecImport

data Demo' f = Demo
  { foo :: HKD f Int,
    bar :: HKD f Text
  }
  deriving (Generic)

type Demo = Demo' Identity

deriving instance Show Demo

deriving instance Eq Demo

type DemoText = Demo' (Const Text)

deriving instance Show DemoText

deriving instance Eq DemoText

data Key = F | G | A | Bb
  deriving (Show, Eq, Ord, Bounded, Enum)

parser :: Demo' Parser
parser =
  Demo
    { foo = decimal,
      bar = string "left" <|> string "right"
    }

spec :: Spec
spec = do
  describe "enumParser" $ do
    it "parses single characters" $
      parseOnly enumParser "f" `shouldBe` Right F

    it "parses multiple characters" $
      parseOnly enumParser "bb" `shouldBe` Right Bb

    it "can fail" $
      parseOnly enumParser "x" `shouldBe` (Left "Failed reading: empty" :: Either String Key)

  describe "fields" $ do
    it "lists field names" $ do
      fields parser `shouldBe` Set.fromList ["foo", "bar"]

  describe "parse" $
    it "applies a parser to structured input" $
      parse parser (Demo "123" "right") `shouldBe` Right (Demo 123 "right")

  describe "parseStruct" $ do
    let run :: [(Text, Text)] -> Either String Demo
        run = parseStruct parser . Map.fromList

    it "can parse valid data" $ do
      run [("foo", "123"), ("bar", "right")] `shouldBe` Right (Demo 123 "right")
      run [("foo", "456"), ("bar", "left")] `shouldBe` Right (Demo 456 "left")

    it "can fail to parse" $ do
      -- TODO: better error messages here
      run [("foo", "abc"), ("bar", "right")] `shouldBe` Left "Failed reading: takeWhile1"
      run [("foo", "123"), ("bar", "top")] `shouldBe` Left "string"

  describe "struct" $ do
    it "parses from a map" $ do
      let parsed :: Maybe DemoText
          parsed = struct $ Map.fromList [("foo", "a"), ("bar", "b")]

      parsed `shouldBe` Just (Demo {foo = "a", bar = "b"})

  describe "validate" $ do
    it "passes if all right" $
      validate (Demo (Right 1) (Right "a")) `shouldBe` Right (Demo 1 "a")

    it "gathers errors" $
      validate (Demo (Left "first") (Left "second")) `shouldBe` Left (Map.fromList [("foo", "first"), ("bar", "second")])
