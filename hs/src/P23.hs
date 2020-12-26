module P23 where

import Data.Char (digitToInt)
import qualified Data.Vector.Unboxed.Mutable as MV
import Import hiding (from, rotate, shift, to)

data Cups m = Cups
  { bound :: Int,
    current :: Int,
    successors :: MV.MVector (PrimState m) Int
  }

solution :: Solution [Int]
solution = solve parser $ \segment -> do
  part1 $
    runST $ do
      cups <- initialize (length segment) segment
      steps 100 cups
      label cups

  part2 $
    runST $ do
      cups <- initialize 1000000 segment
      steps 10000000 cups
      n1 <- MV.read (successors cups) 1
      n2 <- MV.read (successors cups) n1
      return $ n1 * n2

steps :: PrimMonad m => Int -> Cups m -> m (Cups m)
steps 1 c = step c
steps n c = step c >>= steps (n - 1)

step :: PrimMonad m => Cups m -> m (Cups m)
step cups@Cups {..} = do
  (a, b, c, d) <- nextFour current successors
  let destination = until (\n -> n /= a && n /= b && n /= c) (dec cups) $ dec cups current
  afterDestination <- MV.read successors destination
  MV.write successors current d
  MV.write successors destination a
  MV.write successors c afterDestination
  return $ cups {current = d}

nextFour :: PrimMonad m => Int -> MV.MVector (PrimState m) Int -> m (Int, Int, Int, Int)
nextFour n0 v = do
  n1 <- MV.read v n0
  n2 <- MV.read v n1
  n3 <- MV.read v n2
  n4 <- MV.read v n3
  return (n1, n2, n3, n4)

dec :: Cups m -> Int -> Int
dec Cups {..} 1 = bound
dec _ n = n - 1

label :: PrimMonad m => Cups m -> m Text
label Cups {..} = go ("", 1)
  where
    go (acc, i) = do
      next <- MV.read successors i
      if next == 1
        then return acc
        else go (acc <> show next, next)

parser :: Parser [Int]
parser = some (fmap digitToInt digit)

initialize :: PrimMonad m => Int -> [Int] -> m (Cups m)
initialize size segment = do
  successors <- MV.replicate (size + 1) (-1)
  f successors 1
  g successors segment 1
  return $ Cups size (fromMaybe 0 $ head segment) successors
  where
    f v i
      | i == size = MV.write v i (fromMaybe 1 $ head segment)
      | otherwise = MV.write v i (i + 1) >> f v (i + 1)
    g v (a : b : rest) i = MV.write v a b >> g v (b : rest) (i + 1)
    g v [a] i
      | i == size = MV.write v a (fromMaybe 1 $ head segment)
      | otherwise = MV.write v a (i + 1)
    g _ _ _ = return ()

eachCons :: [a] -> [(a, a)]
eachCons (a : b : rest) = (a, b) : eachCons (b : rest)
eachCons _ = []
