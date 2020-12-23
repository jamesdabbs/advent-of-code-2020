module P18 where

import qualified Data.Sequence as Seq (fromList, singleton)
import qualified Data.Text as Text
import Import hiding (evaluate, exp)

data Expression
  = Term Int
  | Plus
  | Times
  | Parens (Seq Expression)
  deriving (Show, Eq)

data AST
  = Node Int
  | Add [AST]
  | Mult [AST]
  deriving (Show, Eq)

newtype SyntaxError = SyntaxError Text
  deriving (Show, Eq)

solution :: Solution [Expression]
solution = solve (parser `sepBy` "\n") $ \input -> do
  part1 $ process associate input -- Right 12956356593940
  part2 $ process assocPlus input -- Right 94240043727614

process :: Traversable f => (Seq Expression -> Either SyntaxError AST) -> f Expression -> Either SyntaxError Int
process assoc input = sum . map evaluate <$> mapM (assoc `groupsIn`) input

walk ::
  (Seq Expression -> Seq Expression -> Either SyntaxError AST) ->
  Seq Expression ->
  Either SyntaxError AST
walk adder (a :|> Plus :|> b) = adder a (Seq.singleton b)
walk adder (a :|> Times :|> b) = mult <$> walk adder a <*> walk adder (Seq.singleton b)
walk adder (Empty :|> Parens exps) = walk adder exps
walk _ (Empty :|> Term n) = Right $ Node n
walk _ exp = Left $ SyntaxError $ "walk failed at " <> renderExp (Parens exp)

associate :: Seq Expression -> Either SyntaxError AST
associate = walk $ \a b -> add <$> associate a <*> associate b

assocPlus :: Seq Expression -> Either SyntaxError AST
assocPlus = walk $ \a b -> case gatherPlus (toList b) a of
  Left err -> Left err
  Right (addends, Empty) -> Add <$> mapM (assocPlus `groupsIn`) addends
  Right (addends, tail) -> mult <$> (Add <$> mapM (assocPlus `groupsIn`) addends) <*> assocPlus tail

gatherPlus :: [Expression] -> Seq Expression -> Either SyntaxError ([Expression], Seq Expression)
gatherPlus acc (a :|> Plus :|> b) = gatherPlus (b : acc) a
gatherPlus acc (a :|> Times :|> b) = Right (b : acc, a)
gatherPlus acc (Empty :|> b) = Right (b : acc, Empty)
gatherPlus _ exps = Left $ SyntaxError $ "gatherPlus failed at " <> renderExp (Parens exps)

groupsIn :: (Seq Expression -> Either SyntaxError AST) -> Expression -> Either SyntaxError AST
groupsIn grouping (Parens exps) = grouping exps
groupsIn _ (Term n) = Right $ Node n
groupsIn _ exp = Left $ SyntaxError $ "no groups in (" <> renderExp exp <> ")"

evaluate :: AST -> Int
evaluate (Node n) = n
evaluate (Add nodes) = sum $ map evaluate nodes
evaluate (Mult nodes) = product $ map evaluate nodes

add :: AST -> AST -> AST
add a b = Add [a, b]

mult :: AST -> AST -> AST
mult a b = Mult [a, b]

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
