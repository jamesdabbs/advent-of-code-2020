module P07 where

import Import hiding (takeWhile)
import Data.Attoparsec.Text (anyChar, manyTill)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Lens ( (%=), at )

type Rule = (Text, [(Int, Text)])

solve :: Text -> IO ()
solve input = do
  rules <- parse parser input

  print $ Set.size $ containers rules "shiny gold" -- 121
  print $ size rules "shiny gold" -- 3805

containers :: [Rule] -> Text -> Set Text
containers rules = expand Set.empty . parents
  where
    -- bag => [bag] that contain it
    index = flip execState Map.empty $
      forM rules $ \(outer, inners) ->
        forM inners $ \(_, inner) ->
          at inner %= Just . maybe [outer] (outer :)

    parents bag = Map.findWithDefault [] bag index

    expand seen (a:as)
      | a `Set.member` seen = expand seen as
      | otherwise = expand (a `Set.insert` seen) (parents a <> as)
    expand seen _ = seen

size :: [Rule] -> Text -> Int
size rules = go
  where
    -- bag => [(count, bag)] within it
    index = Map.fromList rules

    go bag = sum $ map (\(n, inner) -> n * (1 + go inner)) $ Map.findWithDefault [] bag index

parser :: Parser [Rule]
parser = line `sepBy` "\n"
  where
    bag = Text.pack <$> manyTill anyChar " bag"

    line = (,)
      <$> bag <* "s contain "
      <*> (nothing <|> (content `sepBy` ", ")) <* "."

    content = (,)
      <$> decimal <* " "
      <*> bag <* optional "s"

    nothing = "no other bags" $> []