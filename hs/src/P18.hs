module P18 where

import Data.Attoparsec.Text
import Data.Sequence as Seq
import Import hiding (evaluate, exp, group)
import Prelude (error)

data Expression
  = Term Int
  | Plus
  | Times
  | Parens (Seq Expression)
  deriving (Show, Eq)

data AST
  = Node Int
  | Add (Seq AST)
  | Mult (Seq AST)
  deriving (Show, Eq)

solution :: Solution [Expression]
solution = solve (parser `sepBy` "\n") $ \input -> do
  part1 $ sum $ map (evaluate . toAST associate) input -- 12956356593940
  part2 $ sum $ map (evaluate . toAST associate . withGroups groupPlus) input -- 94240043727614

withGroups :: (Seq Expression -> Seq Expression) -> Expression -> Expression
withGroups grouping (Parens exps) = Parens $ grouping exps
withGroups _ exp = exp

groupPlus :: Seq Expression -> Seq Expression
groupPlus se@(_ :<| Plus :<| _) =
  let (addends, tail) = split Times se
      head' = Parens (map (withGroups groupPlus) addends)
      tail' = case tail of
        (Times :<| tail'') -> Times :<| groupPlus tail''
        _ -> Empty
   in head' :<| tail'
groupPlus (Parens exps :<| as) = Parens (groupPlus exps) :<| groupPlus as
groupPlus (a :<| as) = a :<| groupPlus as
groupPlus Empty = Empty

toAST :: (Seq Expression -> AST) -> Expression -> AST
toAST grouping (Parens exps) = grouping exps
toAST _ (Term n) = Node n
toAST _ exp = error ("toAST:" <> show exp)

associate :: Seq Expression -> AST
associate (a :|> Plus :|> b) = Add (Empty :|> associate a :|> toAST associate b)
associate (a :|> Times :|> b) = Mult (Empty :|> associate a :|> toAST associate b)
associate (Empty :|> b) = toAST associate b
associate e = error ("associate:" <> show e)

evaluate :: AST -> Int
evaluate (Node n) = n
evaluate (Add nodes) = sum $ map evaluate nodes
evaluate (Mult nodes) = product $ map evaluate nodes

split :: Eq a => a -> Seq a -> (Seq a, Seq a)
split delim = go Empty
  where
    go acc Empty = (acc, Empty)
    go acc tail@(a :<| rest)
      | a == delim = (acc, tail)
      | otherwise = go (acc :|> a) rest

parser :: Parser Expression
parser = Parens . Seq.fromList <$> expression `sepBy` " "
  where
    expression =
      choice
        [ Term <$> decimal,
          Plus <$ "+",
          Times <$ "*",
          "(" *> parser <* ")"
        ]
