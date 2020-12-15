module P14 where

import Data.Attoparsec.Text (anyChar, count)
import qualified Data.Map as Map
import Import hiding (mask)

type Input = [MaskGroup]

type MaskGroup = (Mask, [(Int64, Int64)])

type Mask = [(Int, Char)]

type RAM = Map Int64 Int64

type Strategy = Mask -> RAM -> (Int64, Int64) -> RAM

data Bit = O | I deriving (Show, Eq)

solution :: Solution Input
solution = solve parser $ \input -> do
  part1 $ sum $ process v1 input -- 12408060320841
  part2 $ sum $ process v2 input -- 4466434626828

process :: Strategy -> Input -> RAM
process apply = foldl' processGroup Map.empty
  where
    processGroup acc (mask, sets) = foldl' (apply mask) acc sets

v1 :: Strategy
v1 mask ram (addr, value) = Map.insert addr (masked mask value) ram

masked :: Mask -> Int64 -> Int64
masked mask n = foldl' bitmask n mask
  where
    bitmask acc (i, '0') = acc `clearBit` i
    bitmask acc (i, '1') = acc `setBit` i
    bitmask acc _ = acc

v2 :: Strategy
v2 mask ram (addr, value) = Map.fromList [(key, value) | key <- floats mask addr] <> ram

floats :: Mask -> Int64 -> [Int64]
floats mask base = foldM f base mask
  where
    f n (i, 'X') = [n `setBit` i, n `clearBit` i]
    f n (i, '1') = [n `setBit` i]
    f n _ = [n]

parser :: Parser Input
parser = commands `sepBy` "\n"
  where
    commands =
      (,)
        <$> ("mask = " *> mask <* "\n")
        <*> set `sepBy` "\n"

    mask = zip [width - 1, width - 2 .. 0] <$> count width anyChar

    set =
      (,)
        <$> ("mem[" *> decimal <* "] = ")
        <*> decimal

width :: Int
width = 36