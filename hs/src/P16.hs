module P16 where

import Data.Attoparsec.Text (sepBy1, takeTill)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Import hiding (check, sepBy1)

data Input = Input
  { rules :: [Rule],
    yours :: Ticket,
    others :: [Ticket]
  }
  deriving (Show, Eq)

data Rule = Rule
  { label :: Text,
    r1 :: Range,
    r2 :: Range
  }
  deriving (Show, Eq)

type Ticket = [Int]

type Range = (Int, Int)

solution :: Solution Input
solution = solve parser $ \Input {..} -> do
  let (validTickets, errors) = validate rules others

  part1 $ sum errors -- 30869
  part2 $ score "departure" yours <$> greedyPick (ruleOut rules validTickets) -- Just 4381476149273

validate :: [Rule] -> [Ticket] -> ([Ticket], [Int])
validate rules = foldl (f ranges) ([], [])
  where
    ranges = foldl (\acc (Rule _ a b) -> a : b : acc) [] rules

    f rs (ts, es) ticket = case filter (\n -> not $ any (cover n) rs) ticket of
      [] -> (ticket : ts, es)
      errs -> (ts, es <> errs)

{- Finds an index => label match, assuming there is always one index that is
   uniquely constrained (after removing labels that have already been chosen)
-}
greedyPick :: Ord a => [[a]] -> Maybe [a]
greedyPick =
  zip [0 ..] -- annotate with current indices
    >>> sortOn (length . snd) -- process from most => least constrained
    >>> foldl process (Just Map.empty) -- pick the unpicked element
    >>> fmap (map fst . sortOn snd . Map.toList) -- extract the solution and restore the initial order
  where
    process :: Ord a => Maybe (Map a Int) -> (Int, [a]) -> Maybe (Map a Int)
    process (Just picks) (ix, as) = case find (\a -> not $ Map.member a picks) as of
      Just a -> Just $ Map.insert a ix picks
      _ -> Nothing
    process _ _ = Nothing

ruleOut :: [Rule] -> [Ticket] -> [[Text]]
ruleOut rules = map (map label . check rules) . transpose
  where
    check acc (n : ns) = check (filter (holds n) acc) ns
    check acc [] = acc

score :: Text -> Ticket -> [Text] -> Int
score prefix ticket = product . zipWith point ticket
  where
    point n label
      | prefix `Text.isPrefixOf` label = n
      | otherwise = 1

holds :: Int -> Rule -> Bool
holds n Rule {..} = cover n r1 || cover n r2

cover :: Int -> Range -> Bool
cover x (a, b) = x >= a && x <= b

parser :: Parser Input
parser = do
  rules <- rule `sepBy` "\n"
  "\n\nyour ticket:\n"
  yours <- ticket
  "\n\nnearby tickets:\n"
  others <- ticket `sepBy` "\n"
  return Input {..}
  where
    rule = do
      label <- takeTill (== ':') <* ": "
      r1 <- range <* " or "
      r2 <- range
      return Rule {..}

    range = (,) <$> (decimal <* "-") <*> decimal

    ticket = decimal `sepBy1` ","
