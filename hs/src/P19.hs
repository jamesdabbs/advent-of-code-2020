module P19 where

import Data.Attoparsec.Text (takeTill)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Import

type Input = (Rules, Set Text)

type Rules = Map Int Rule

data Rule
  = Compound (NonEmpty Int) (NonEmpty Int)
  | Simple (NonEmpty Int)
  | Literal Char
  deriving (Show, Eq)

solution :: Solution Input
solution = solve parser $ \(rules, messages) -> do
  let expanded = expand rules 0

  part1 $ Set.size $ expanded `Set.intersection` messages -- 182
  part2 $ Set.size $ Set.filter (patches (expand rules 42) (expand rules 31) expanded) messages -- 334

matches :: Rules -> Set Text -> Int
matches rules messages = Set.size $ expand rules 0 `Set.intersection` messages

patches :: Set Text -> Set Text -> Set Text -> Text -> Bool
patches pres posts messages message =
  message `Set.member` messages || any (patches pres posts messages) (reductions message)
  where
    reductions m = withoutPres m <> concatMap withoutPosts (withoutPres m)

    withoutPres m = mapMaybe (strip Text.isPrefixOf Text.drop m) (toList pres)
    withoutPosts m = mapMaybe (strip Text.isSuffixOf Text.dropEnd m) (toList posts)

    strip matcher dropper haystack needle =
      if needle `matcher` haystack
        then Just $ dropper (Text.length needle) haystack
        else Nothing

expand :: Rules -> Int -> Set Text
expand rules n = case Map.lookup n rules of
  Nothing -> Set.empty
  Just (Literal c) -> Set.singleton $ Text.singleton c
  Just (Simple ns) -> combine $ map (expand rules) ns
  Just (Compound as bs) -> Set.union (combine $ map (expand rules) as) (combine $ map (expand rules) bs)

combine :: NonEmpty (Set Text) -> Set Text
combine (s :| ss) = foldl f s ss
  where
    f l r = Set.fromList [a <> b | a <- toList l, b <- toList r]

parser :: Parser Input
parser =
  (,)
    <$> fmap Map.fromList (numbered rule `sepBy` char '\n')
    <* "\n\n"
    <*> fmap Set.fromList (takeTill (== '\n') `sepBy` char '\n')
  where
    numbered p = (,) <$> decimal <* ": " <*> p

    rule =
      choice
        [ Literal <$> (char '"' *> anyChar <* char '"'),
          Compound <$> numbers <* " | " <*> numbers,
          Simple <$> numbers
        ]

    numbers = decimal `sepBy1` " "
