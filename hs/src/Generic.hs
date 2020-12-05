{-# LANGUAGE DefaultSignatures #-}
module Generic
  ( HKD
  , fields
  , parseStruct
  ) where

import Protolude

import           Data.Attoparsec.Text (Parser, endOfInput, parseOnly)
import           Data.String          (String)
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import           GHC.Generics         ()

{- See https://reasonablypolymorphic.com/blog/higher-kinded-data for a great
  writeup of the sort of generic programming present in this file, and the
  motivation behind it.
-}
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

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


{- Defines a generic function for applying a struct of parsers to produce a
   struct of the same type

   parseStruct
     (Struct { a = parseA, b = parseB })
     (Map.fromList [("a", "..."), ("b", "...")])
     = Just (Struct { a = ..., b = ... }) -- if parsing succeeds
       Nothing                            -- if parsing fails
-}
parseStruct :: ( Generic (f Parser)
               , Generic (f Identity)
               , GParse (Rep (f Parser)) (Rep (f Identity))
               )
            => f Parser -> Map Text Text -> Maybe (f Identity)
parseStruct parser = fmap to . gparse (from parser)

class GParse i o where
  gparse :: i p -> Map Text Text -> Maybe (o p)

instance (GParse i o, GParse i' o') => GParse (i :*: i') (o :*: o') where
  gparse (l :*: r) fs = (:*:) <$> gparse l fs <*> gparse r fs

instance (GParse i o, GParse i' o') => GParse (i :+: i') (o :+: o') where
  gparse (L1 l) fs = L1 <$> gparse l fs
  gparse (R1 r) fs = R1 <$> gparse r fs

instance (GParse i o) => GParse (D1 b i) (D1 b' o) where
  gparse (M1 x) fs = M1 <$> gparse x fs

instance (GParse i o) => GParse (C1 b i) (C1 b' o) where
  gparse (M1 x) fs = M1 <$> gparse x fs

instance Selector s => GParse (S1 s (K1 a (Parser p))) (S1 s (K1 a p)) where
  gparse (M1 (K1 parser)) fs = (M1 . K1) <$> runParser parser label fs
    where
      label = selName (undefined :: S1 s (K1 a (Parser p)) ())

runParser :: Parser p -> String -> Map Text Text -> Maybe p
runParser parser key fs =
  either (const Nothing) Just
  $ parseOnly (parser <* endOfInput)
  $ maybe "" identity
  $ Map.lookup (Text.pack key) fs
