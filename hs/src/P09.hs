module P09 where

import Import
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq


solution :: Solution (Seq Int)
solution = solve parser $ \numbers -> do
  let m = scanForNonSum 25 numbers
  part1 m -- Just 70639851

  part2 $ fmap (uncurry (+)) $ -- Just 8249240
    bounds =<< flip consecutiveSum numbers =<< m

parser :: Parser (Seq Int)
parser = Seq.fromList <$> decimal `sepBy` "\n"

scanForNonSum :: Int -> Seq Int -> Maybe Int
scanForNonSum size = uncurry go . Seq.splitAt size
  where
    -- TODO: we can certainly get better asymptotics than this, but the constants
    --       here are relatively small. Benchmark _then_ optimize.
    go pre@(_ :<| ps) (a :<| as) = if a `elem` liftA2 (+) pre pre
      then go (ps |> a) as
      else Just a
    go _ _ = Nothing

bounds :: Seq Int -> Maybe (Int, Int)
bounds Empty = Nothing
bounds (a :<| as) = Just $ foldr (\n -> min n *** max n) (a, a) as

consecutiveSum :: Int -> Seq Int -> Maybe (Seq Int)
consecutiveSum _ Empty = Nothing
consecutiveSum target s@(_ :<| ns) = takeSum target s <|> consecutiveSum target ns

takeSum :: Int -> Seq Int -> Maybe (Seq Int)
takeSum target = go Seq.empty 0
  where
    go _ _ Empty = Nothing
    go acc total (n :<| ns) = case compare total target of
      LT -> go (acc |> n) (total + n) ns
      EQ -> Just acc
      GT -> Nothing -- Assuming non-negative integers, we can bail once we're over the target
