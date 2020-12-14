module P14 where

import qualified Data.Map as Map
import Import hiding (mask)

type Input = [MaskGroup]

type MaskGroup = (Mask, [(Int, Int)])

type Mask = [(Int, Maybe Bit)]

type RAM = Map Int Int

data Bit = O | I deriving (Show, Eq)

solution :: Solution Input
solution = solve parser $ \input -> do
  part1 $ sum $ process v1 input -- 12408060320841
  part2 $ sum $ process v2 input -- 4466434626828

process :: (Mask -> RAM -> (Int, Int) -> RAM) -> Input -> RAM
process apply = foldl processGroup Map.empty
  where
    processGroup acc (mask, sets) = foldl (apply mask) acc sets

v1 :: Mask -> RAM -> (Int, Int) -> RAM
v1 mask ram (addr, value) = Map.insert addr (masked mask value) ram

masked :: Mask -> Int -> Int
masked mask n = foldr bitmask n mask
  where
    bitmask (i, Just O) = flip clearBit i
    bitmask (i, Just I) = flip setBit i
    bitmask _ = identity

v2 :: Mask -> RAM -> (Int, Int) -> RAM
v2 mask ram (addr, value) = foldl (\r i -> Map.insert i value r) ram $ floats mask addr

floats :: Mask -> Int -> [Int]
floats mask base = foldM f base mask
  where
    f n (i, Nothing) = [n `setBit` i, n `clearBit` i]
    f n (i, Just I) = [n `setBit` i]
    f n _ = [n]

parser :: Parser Input
parser = commands `sepBy` "\n"
  where
    commands =
      (,)
        <$> ("mask = " *> mask <* "\n")
        <*> set `sepBy` "\n"

    mask =
      fmap (zip [width - 1, width - 2 .. 0]) $
        many $
          choice
            [ "X" $> Nothing,
              "0" $> Just O,
              "1" $> Just I
            ]

    set =
      (,)
        <$> ("mem[" *> decimal <* "] = ")
        <*> decimal

width :: Int
width = 36