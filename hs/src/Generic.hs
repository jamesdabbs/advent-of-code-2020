module Generic
  ( HKD,
    Messages,
    enumParser,
    fields,
    parse,
    parseStruct,
    struct,
    validate,
  )
where

import Data.Attoparsec.Text (Parser, choice, endOfInput, parseOnly, string)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (String)
import qualified Data.Text as Text
import GHC.Generics ()
import Protolude

type Messages = Map String String

enumParser :: (Bounded e, Enum e, Show e) => Parser e
enumParser = choice $ map opt [minBound .. maxBound]
  where
    opt e = string (Text.toLower $ show e) $> e

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
  (Generic (f p), GStruct (Rep (f p))) =>
  Map Text Text ->
  Either Messages (f p)
struct fs = to <$> gstruct fs

class GStruct o where
  gstruct :: Map Text Text -> Either Messages (o p)

instance Selector s => GStruct (S1 s (Rec0 (Const Text a))) where
  gstruct fs = case Map.lookup (Text.pack label) fs of
    Just value -> Right $ M1 $ K1 $ Const value
    _ -> Left $ Map.singleton label "not found"
    where
      label = selName (undefined :: S1 s (Rec0 Text) ())

instance Selector s => GStruct (S1 s (Rec0 (Maybe (Const Text a)))) where
  gstruct fs = Right . M1 . K1 $ Const <$> Map.lookup (Text.pack label) fs
    where
      label = selName (undefined :: S1 s (Rec0 Text) ())

instance (GStruct i, GStruct i') => GStruct (i :*: i') where
  gstruct fs = case (gstruct fs, gstruct fs) of
    (Right o, Right o') -> Right (o :*: o')
    (Left s, Left s') -> Left (s <> s')
    (Left s, _) -> Left s
    (_, Left s') -> Left s'

instance (GStruct i) => GStruct (D1 b i) where
  gstruct fs = M1 <$> gstruct fs

instance (GStruct i) => GStruct (C1 b i) where
  gstruct fs = M1 <$> gstruct fs

{- validate allows us to collapse a struct-of-eithers into a collection of
   tagged error messages if there are any, or a struct-of-rights if not.
-}
validate ::
  ( GValidate (Rep (f (Either String))) (Rep (f Identity)),
    Generic (f (Either String)),
    Generic (f Identity)
  ) =>
  f (Either String) ->
  Either Messages (f Identity)
validate i = to <$> gvalidate (from i)

class GValidate i o where
  gvalidate :: i x -> Either Messages (o x)

instance Selector s => GValidate (S1 s (Rec0 (Either String a))) (S1 s (Rec0 a)) where
  gvalidate (M1 (K1 (Right x))) = Right $ M1 $ K1 x
  gvalidate (M1 (K1 (Left s))) = Left $ Map.singleton label s
    where
      label = selName (undefined :: S1 s (Rec0 (Either String a)) ())

instance Selector s => GValidate (S1 s (Rec0 (Maybe (Either String a)))) (S1 s (Rec0 (Maybe a))) where
  gvalidate (M1 (K1 Nothing)) = Right $ M1 $ K1 Nothing
  gvalidate (M1 (K1 (Just (Right x)))) = Right $ M1 $ K1 $ Just x
  gvalidate (M1 (K1 (Just (Left s)))) = Left $ Map.singleton label s
    where
      label = selName (undefined :: S1 s (Rec0 (Either String a)) ())

instance (GValidate i o) => GValidate (D1 a i) (D1 a' o) where
  gvalidate (M1 x) = M1 <$> gvalidate x

instance (GValidate i o) => GValidate (C1 a i) (C1 a' o) where
  gvalidate (M1 x) = M1 <$> gvalidate x

instance (GValidate i o, GValidate i' o') => GValidate (i :*: i') (o :*: o') where
  gvalidate (i :*: i') = case (gvalidate i, gvalidate i') of
    (Right o, Right o') -> Right (o :*: o')
    (Left s, Left s') -> Left (s <> s')
    (Left s, _) -> Left s
    (_, Left s') -> Left s'

{- parse gives us a generic means of applying a struct-of-parsers to a similar
   struct-of-inputs to get a struct-of-results
-}
parse ::
  ( GParse (Rep (f Parser)) (Rep (f (Const Text))) (Rep (f (Either String))),
    Generic (f Parser),
    Generic (f (Const Text)),
    Generic (f (Either String))
  ) =>
  f Parser ->
  f (Const Text) ->
  f (Either String)
parse parser input = to $ gparse (from parser) (from input)

class GParse p i o where
  gparse :: p x -> i x -> o x

instance GParse (Rec0 (Parser a)) (Rec0 (Const Text a)) (Rec0 (Either String a)) where
  gparse (K1 parser) (K1 (Const input)) = K1 $ run parser input

instance GParse (Rec0 (Maybe (Parser a))) (Rec0 (Maybe (Const Text a))) (Rec0 (Maybe (Either String a))) where
  gparse (K1 (Just parser)) (K1 (Just (Const input))) = K1 $ Just $ run parser input
  gparse _ _ = K1 Nothing

instance (GParse p i o) => GParse (M1 a b p) (M1 a' b' i) (M1 a'' b'' o) where
  gparse (M1 p) (M1 i) = M1 $ gparse p i

instance (GParse p i o, GParse p' i' o') => GParse (p :*: p') (i :*: i') (o :*: o') where
  gparse (p :*: p') (i :*: i') = gparse p i :*: gparse p' i'

run :: Parser a -> Text -> Either String a
run parser = parseOnly (parser <* endOfInput)

parseStruct ::
  ( GStruct (Rep (f (Const Text))),
    GValidate (Rep (f (Either String))) (Rep (f Identity)),
    GParse (Rep (f Parser)) (Rep (f (Const Text))) (Rep (f (Either String))),
    Generic (f (Const Text)),
    Generic (f (Either String)),
    Generic (f Parser),
    Generic (f Identity)
  ) =>
  f Parser ->
  Map Text Text ->
  Either (Map String String) (f Identity)
parseStruct parser fs =
  validate . parse parser =<< struct fs
