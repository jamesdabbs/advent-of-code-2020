module P22 where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Import

type Deck = Seq Int

type Decks = (Deck, Deck)

solution :: Solution Decks
solution = solve parser $ \decks -> do
  part1 $ uncurry (+) $ score *** score $ combat decks -- 31754
  part2 $ score $ let (_, deck, _) = recursiveCombat Set.empty Map.empty decks in deck

combat :: Decks -> Decks
combat (a :<| as, b :<| bs)
  | a >= b = combat (as :|> a :|> b, bs)
  | otherwise = combat (as, bs :|> b :|> a)
combat (a, b) = (a, b)

recursiveCombat :: Set Decks -> Map Decks Bool -> Decks -> (Bool, Deck, Map Decks Bool)
recursiveCombat history subgames i@(a :<| as, b :<| bs) =
  let next :: Bool -> Map Decks Bool -> (Bool, Deck, Map Decks Bool)
      next True subs = recursiveCombat (i `Set.insert` history) subs (as :|> a :|> b, bs)
      next False subs = recursiveCombat (i `Set.insert` history) subs (as, bs :|> b :|> a)
   in if i `Set.member` history -- if input has been seen
        then (True, Seq.empty, Map.insert i True subgames) -- a wins immediately; record the loop as a win
        else case Map.lookup i subgames of
          Just result -> (result, Seq.empty, subgames) -- we've played this game before; return the recorded result
          _ ->
            if length as >= a && length bs >= b -- can play a recursive game
              then
                let as' = Seq.take a as
                    bs' = Seq.take b bs
                    (winner, _, subgames') = recursiveCombat Set.empty subgames (as', bs')
                 in next winner (Map.insert (as', bs') winner subgames') -- keep going, recording this subgame and its transient subgames
              else next (a >= b) subgames
recursiveCombat _ subgames (a, Empty) = (True, a, subgames)
recursiveCombat _ subgames (Empty, b) = (False, b, subgames)

score :: Deck -> Int
score = go 0 1
  where
    go acc fact (cs :|> c) = go (acc + fact * c) (fact + 1) cs
    go acc _ Empty = acc

parser :: Parser Decks
parser =
  (,)
    <$> ("Player 1:\n" *> deck)
    <*> ("\n\nPlayer 2:\n" *> deck)
  where
    deck = Seq.fromList <$> decimal `sepBy` "\n"
