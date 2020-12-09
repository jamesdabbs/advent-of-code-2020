module P09 where

import Import

solve :: Text -> IO ()
solve input = do
  numbers <- parse (decimal `sepBy` "\n") input

  let Just n = scan 25 numbers
  print n -- 70639851
  print $ boundSum <$> conSum n numbers -- Just 8249240

scan :: Int -> [Int] -> Maybe Int
scan size = uncurry go . splitAt size
  where
    go pre@(_:ps) (a:as) = if a `elem` [ x + y | x <- pre, y <- pre]
      then go (ps <> [a]) as
      else Just a
    go _ _ = Nothing

boundSum :: [Int] -> Int
boundSum ns = minimum ns + maximum ns

conSum :: Int -> [Int] -> Maybe [Int]
conSum _ [] = Nothing
conSum target l@(_:ns) = case takeSum target l of
  Just s -> Just s
  Nothing -> conSum target ns

takeSum :: Int -> [Int] -> Maybe [Int]
takeSum target = go [] 0
  where
    go :: [Int] -> Int -> [Int] -> Maybe [Int]
    go _ _ [] = Nothing
    go acc total (n : ns)
      | total + n == target = Just (acc <> [n])
      | otherwise = go (acc <> [n]) (total + n) ns
