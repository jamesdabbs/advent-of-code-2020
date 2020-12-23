module P09 where

import qualified Data.Sequence as Seq
import Import
import P01 (findPair)

solution :: Solution (Seq Int)
solution = solve parser $ \numbers -> do
  let m = scanForNonSum 25 numbers
  part1 m -- Just 70639851
  part2 $
    fmap (uncurry (+)) $ -- Just 8249240
      bounds =<< flip consecutiveSum numbers =<< m

parser :: Parser (Seq Int)
parser = Seq.fromList <$> decimal `sepBy` "\n"

scanForNonSum :: Int -> Seq Int -> Maybe Int
scanForNonSum size = uncurry go . Seq.splitAt size
  where
    go pre@(_ :<| ps) (a :<| as) =
      if isJust $ findPair a $ toList pre
        then go (ps |> a) as
        else Just a
    go _ _ = Nothing

bounds :: Seq Int -> Maybe (Int, Int)
bounds Empty = Nothing
bounds (a :<| as) = Just $ foldr (\n -> min n *** max n) (a, a) as

consecutiveSum :: Int -> Seq Int -> Maybe (Seq Int)
consecutiveSum target = go Empty 0
  where
    -- Since this is a list of non-negative integers, we can maintain a sliding
    -- window which expands right if its sum is too small, and contracts left if
    -- its sum is too high.
    go acc total rest = case compare total target of
      EQ -> Just acc
      LT -> case rest of
        (n :<| ns) -> go (acc |> n) (total + n) ns
        _ -> Nothing
      GT -> case acc of
        (a :<| as) -> go as (total - a) rest
        _ -> Nothing
