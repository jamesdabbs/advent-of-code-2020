module P18 where

import Data.Attoparsec.Text
import Data.Sequence as Seq
import qualified Data.Text as Text
import Import hiding (evaluate, exp, group)

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

newtype SyntaxError = SyntaxError Text
  deriving (Show, Eq)

solution :: Solution [Expression]
solution = solve (parser `sepBy` "\n") $ \input -> do
  part1 $ process (toAST associate) input -- Right 12956356593940
  part2 $ process (toAST associate . withGroups groupPlus) input -- Right 94240043727614

process :: (Expression -> Either SyntaxError AST) -> [Expression] -> Either SyntaxError Int
process p input = sum . map evaluate <$> mapM p input

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

toAST :: (Seq Expression -> Either SyntaxError AST) -> Expression -> Either SyntaxError AST
toAST grouping (Parens exps) = grouping exps
toAST _ (Term n) = Right $ Node n
toAST _ exp = Left $ SyntaxError $ "cannot cast (" <> renderExp exp <> ") to AST"

associate :: Seq Expression -> Either SyntaxError AST
associate (a :|> Plus :|> b) = add <$> associate a <*> toAST associate b
associate (a :|> Times :|> b) = mult <$> associate a <*> toAST associate b
associate (Empty :|> b) = toAST associate b
associate exp = Left $ SyntaxError $ "cannot associate without an operator " <> renderExp (Parens exp)

evaluate :: AST -> Int
evaluate (Node n) = n
evaluate (Add nodes) = sum $ map evaluate nodes
evaluate (Mult nodes) = product $ map evaluate nodes

add :: AST -> AST -> AST
add a b = Add $ Empty :|> a :|> b

mult :: AST -> AST -> AST
mult a b = Mult $ Empty :|> a :|> b

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

{- Utilities below are not used for computing the solution, only in test helpers
   and helpful error message generation
-}

renderAST :: AST -> Text
renderAST (Node n) = show n
renderAST (Add ns) = "(" <> Text.intercalate " + " (map renderAST $ toList ns) <> ")"
renderAST (Mult ns) = "(" <> Text.intercalate " * " (map renderAST $ toList ns) <> ")"

renderExp :: Expression -> Text
renderExp (Term n) = show n
renderExp Plus = "+"
renderExp Times = "*"
renderExp (Parens exps) = "(" <> Text.intercalate " " (map renderExp $ toList exps) <> ")"
