module P13 where

import Import hiding (wait)

type Input = (Int, [Maybe Int])

solution :: Solution Input
solution = solve parser $ \(ready, buses) -> do
  part1 $ uncurry (*) $ earliestDeparture ready $ catMaybes buses -- 5257
  part2 $ lineup buses -- 538703333547789

earliestDeparture :: Int -> [Int] -> (Int, Int)
earliestDeparture ready = minimumBy (comparing snd) . map (\b -> (b, wait ready b))
  where
    wait at bus = bus - (at `rem` bus)

lineup :: [Maybe Int] -> Integer
lineup mods =
  let (remainder, modulus) = crt [(i, toInteger n) | (i, Just n) <- zip [0 ..] mods]
   in modulus - remainder

-- From https://stackoverflow.com/questions/35529211
crt :: [(Integer, Integer)] -> (Integer, Integer)
crt = foldr go (0, 1)
  where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- (a `inv` m + a) should be a multiple of m (i.e. = 0 (mod m))
    a `inv` m = let (_, i, _) = gcd' a m in i `mod` m

    -- gcd' a b = (gcd a b, m1, m2) s.t. m1 * a + m2 * b = gcd a b
    gcd' 0 b = (b, 0, 1)
    gcd' a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd' (b `mod` a) a

parser :: Parser Input
parser =
  (,)
    <$> (decimal <* "\n")
    <*> (numbers `sepBy` ",")
  where
    numbers = (Just <$> decimal) <|> ("x" $> Nothing)
