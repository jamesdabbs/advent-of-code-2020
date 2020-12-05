module GenericSpec where

import SpecImport
import Generic

import qualified Data.Map as Map
import qualified Data.Set as Set

data Demo' f = Demo
  { foo :: HKD f Int
  , bar :: HKD f Text
  } deriving Generic

type Demo = Demo' Identity
deriving instance Show Demo
deriving instance Eq Demo

data Key = F | G | A | Bb
  deriving (Show, Eq, Ord, Bounded, Enum)

parser :: Demo' Parser
parser = Demo
  { foo = decimal
  , bar = string "left" <|> string "right"
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

  describe "parseStruct" $ do
    let run :: [(Text, Text)] -> Maybe Demo
        run = parseStruct parser . Map.fromList

    it "can parse valid data" $ do
      run [("foo", "123"), ("bar", "right")] `shouldBe` Just (Demo 123 "right")
      run [("foo", "456"), ("bar", "left" )] `shouldBe` Just (Demo 456 "left")

    it "can fail to parse" $ do
      run [("foo", "abc"), ("bar", "right")] `shouldBe` Nothing
      run [("foo", "123"), ("bar", "top"  )] `shouldBe` Nothing
