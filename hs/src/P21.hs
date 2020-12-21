module P21 where

import Data.Attoparsec.Text (takeWhile1)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Import

type Input = [Food]

type Food = ([Ingredient], [Allergen])

type Ingredients = Set Ingredient

type Ingredient = Text

type Allergen = Text

solution :: Solution Input
solution = solve parser $ \input -> do
  let (ingredients, possibilities) = foldl consider ([], Map.empty) input
      deductions = deduce possibilities

  part1 $ p1 ingredients deductions -- 2317
  part2 $ p2 deductions -- kbdgs,sqvv,slkfgq,vgnj,brdd,tpd,csfmb,lrnz

p1 :: [Ingredient] -> Map Allergen Ingredient -> Int
p1 ingredients deductions =
  let eliminated = Set.fromList $ Map.elems deductions
   in countWhere (\s -> not $ s `Set.member` eliminated) ingredients

p2 :: Map Allergen Ingredient -> Text
p2 deductions = Text.intercalate "," $ map snd $ sortOn fst $ Map.toList deductions

consider :: ([Ingredient], Map Allergen Ingredients) -> Food -> ([Ingredient], Map Allergen Ingredients)
consider (ingredients, possibilities) (is, allergens) =
  ( ingredients <> is,
    foldl (flip $ Map.alter (Just . maybe (Set.fromList is) (Set.intersection $ Set.fromList is))) possibilities allergens
  )

deduce :: Map Allergen Ingredients -> Map Allergen Ingredient
deduce ps = fst $ go (Map.empty, ps)
  where
    go (solutions, possibilities) = case solved possibilities of
      Just (allergen, ingredient) -> go (Map.insert allergen ingredient solutions, remove allergen ingredient possibilities)
      Nothing -> (solutions, possibilities)

    solved possibilities = head $ do
      (allergen, ingredients) <- Map.toList possibilities
      case Set.toList ingredients of
        [ingredient] -> return (allergen, ingredient)
        _ -> empty

    remove allergen ingredient acc = map (Set.delete ingredient) $ Map.delete allergen acc

parser :: Parser Input
parser = line `sepBy` "\n"
  where
    line =
      (,)
        <$> (word `sepBy` " ") <* " (contains "
        <*> (word `sepBy` ", ") <* ")"

    word = takeWhile1 isAlpha
