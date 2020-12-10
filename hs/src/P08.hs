module P08 where

import Data.Attoparsec.Text (signed)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Import

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show, Eq, Ord)

data Cpu = Cpu
  { pc :: Int,
    acc :: Int
  }
  deriving (Show)

type ROM = Map Int Instruction

data Exit = Halt Int | Loop Int deriving (Show, Eq)

solution :: Solution ROM
solution = solve parser $ \rom -> do
  part1 $ run rom -- Loop 1475
  part2 $ findHaltingVariant rom -- Just 1270

step :: Instruction -> Cpu -> Cpu
step (Nop _) (Cpu pc acc) = Cpu (pc + 1) acc
step (Acc n) (Cpu pc acc) = Cpu (pc + 1) (acc + n)
step (Jmp n) (Cpu pc acc) = Cpu (pc + n) acc

run :: ROM -> Exit
run rom = go (Set.singleton 0) (Cpu 0 0)
  where
    go seen cpu = case Map.lookup (pc cpu) rom of
      Nothing -> Halt $ acc cpu
      Just instruction ->
        let next = step instruction cpu
         in if pc next `Set.member` seen
              then Loop $ acc cpu
              else go (pc next `Set.insert` seen) next

mutate :: Instruction -> Instruction
mutate (Acc x) = Acc x
mutate (Nop x) = Jmp x
mutate (Jmp x) = Nop x

variants :: ROM -> [ROM]
variants rom = do
  (k, v) <- Map.toList rom
  let v' = mutate v
  guard $ v /= v'
  return $ Map.insert k v' rom

findHaltingVariant :: ROM -> Maybe Int
findHaltingVariant = head . mapMaybe (toHalt . run) . variants
  where
    toHalt (Halt n) = Just n
    toHalt _ = Nothing

parser :: Parser ROM
parser = Map.fromList . zip [0 ..] <$> instruction `sepBy` "\n"
  where
    instruction =
      choice
        [ Nop <$> ("nop " *> signed decimal),
          Acc <$> ("acc " *> signed decimal),
          Jmp <$> ("jmp " *> signed decimal)
        ]
