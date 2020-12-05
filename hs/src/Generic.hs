module Generic
  ( HKD,
    enumParser,
    fields,
    parse,
    parseStruct,
    struct,
  )
where

import Data.Attoparsec.Text (Parser, choice, endOfInput, parseOnly, string)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (String)
import qualified Data.Text as Text
import GHC.Generics ()
import Protolude

enumParser :: (Bounded e, Enum e, Show e) => Parser e
enumParser = choice $ map opt [minBound .. maxBound]
  where
    opt e = string (Text.toLower $ show e) *> pure e

{- See https://reasonablypolymorphic.com/blog/higher-kinded-data for a great
  writeup of the sort of generic programming present in this file, and the
  motivation behind it.
-}
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

{- Defines a generic function for extracting the names of record types

   fields { a = ..., b = ..., c = ... } = {a, b, c}
-}
fields :: (Generic (f a), GFields (Rep (f a))) => f a -> Set Text
fields = gfields . from

class GFields i where
  gfields :: i p -> Set Text

-- The fields of a product type is the union of the fields of the subtypes
instance (GFields i, GFields i') => GFields (i :*: i') where
  gfields (l :*: r) = gfields l `Set.union` gfields r

-- The fields of a sum type is the fields of the relevant subtype instance
instance (GFields i, GFields i') => GFields (i :+: i') where
  gfields (L1 l) = gfields l
  gfields (R1 r) = gfields r

{- D1 represents a data constructor
   https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html#t:D1
   which does not change the field set
-}
instance (GFields i) => GFields (D1 a i) where
  gfields (M1 x) = gfields x

{- C1 represents a data constructor
   https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html#t:C1
   and similarly does not change the field set
-}
instance (GFields i) => GFields (C1 a i) where
  gfields (M1 x) = gfields x

{- S1 represents a record selector, and Rec0 the base case of structural recursion.
   The field set of a single record selector is the singleton containing the
   fields' name, as read from metadata.
-}
instance (Selector s) => GFields (S1 s (Rec0 i)) where
  gfields (M1 _) = Set.singleton $ Text.pack $ selName (undefined :: S1 s (Rec0 i) ())

{- struct gives us a generic tool for casting from a map to a record where field
   names match the map keys

     struct (Map.fromList [("foo", "a"), ("bar", "b")]) =
       Just (Struct { foo = "a", bar = "b" })
-}
struct ::
  (Generic (f (Const a)), GStruct (Rep (f (Const a)))) =>
  Map Text Text ->
  Maybe (f (Const a))
struct fs = to <$> gstruct fs

class GStruct o where
  gstruct :: Map Text Text -> Maybe (o p)

instance Selector s => GStruct (S1 s (Rec0 Text)) where
  gstruct fs = M1 . K1 <$> Map.lookup label fs
    where
      label = Text.pack $ selName (undefined :: S1 s (Rec0 Text) ())

instance Selector s => GStruct (S1 s (Rec0 (Const Text a))) where
  gstruct fs = M1 . K1 . Const <$> Map.lookup label fs
    where
      label = Text.pack $ selName (undefined :: S1 s (Rec0 Text) ())

instance (GStruct i, GStruct i') => GStruct (i :*: i') where
  gstruct fs = (:*:) <$> gstruct fs <*> gstruct fs

instance (GStruct i) => GStruct (D1 b i) where
  gstruct fs = M1 <$> gstruct fs

instance (GStruct i) => GStruct (C1 b i) where
  gstruct fs = M1 <$> gstruct fs

{- parse gives us a generic means of applying a struct-of-parsers to a similar
   struct-of-inputs to get a struct-of-results
-}
parse ::
  ( GParse (Rep (f Parser)) (Rep (f (Const Text))) (Rep (f Identity)),
    Generic (f Parser),
    Generic (f (Const Text)),
    Generic (f Identity)
  ) =>
  f Parser ->
  f (Const Text) ->
  Either String (f Identity)
parse parser input = to <$> gparse (from parser) (from input)

class GParse p i o where
  gparse :: p x -> i x -> Either String (o x)

instance GParse (Rec0 (Parser a)) (Rec0 Text) (Rec0 a) where
  gparse (K1 parser) (K1 input) = K1 <$> run parser input

instance GParse (Rec0 (Parser a)) (Rec0 (Const Text a)) (Rec0 a) where
  gparse (K1 parser) (K1 (Const input)) = K1 <$> run parser input

instance (GParse p i o) => GParse (M1 a b p) (M1 a' b' i) (M1 a'' b'' o) where
  gparse (M1 p) (M1 i) = M1 <$> gparse p i

instance (GParse p i o, GParse p' i' o') => GParse (p :*: p') (i :*: i') (o :*: o') where
  gparse (p :*: p') (i :*: i') = (:*:) <$> gparse p i <*> gparse p' i'

run :: Parser a -> Text -> Either String a
run parser = parseOnly (parser <* endOfInput)

parseStruct ::
  ( GStruct (Rep (f (Const Text))),
    GParse (Rep (f Parser)) (Rep (f (Const Text))) (Rep (f Identity)),
    Generic (f (Const Text)),
    Generic (f Parser),
    Generic (f Identity)
  ) =>
  f Parser ->
  Map Text Text ->
  Either String (f Identity)
parseStruct parser fs = case struct fs of
  Just input -> parse parser input
  _ -> Left "could not parse structure"
