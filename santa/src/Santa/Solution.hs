module Santa.Solution
  ( Solution,
    Error,
    part1,
    part2,
    run,
    solve,
  )
where

import Control.Lens
import Data.Attoparsec.Text (Parser, endOfInput, parseOnly)
import Data.Text (pack)
import Data.Either.Combinators (mapLeft)
import Protolude

type S = (Text, Text)

newtype SolutionT m a = SolutionT
  { unSolutionT :: StateT S m a
  } deriving (Functor, Applicative, Monad, MonadState S)

data Solution' m a = Solution
  { parser :: Parser a,
    runner :: a -> SolutionT m ()
  }

type Solution = Solution' Identity

newtype Error = ParseError Text
  deriving (Show, Eq, NFData)

run :: Solution a -> Text -> Either Error S
run solution input = runIdentity $ either
  (return . Left . ParseError)
  (\parsed -> Right <$> execStateT (unSolutionT $ runner solution parsed) ("", ""))
  $ parse (parser solution) input

parse :: Parser a -> Text -> Either Text a
parse p i = mapLeft pack $ parseOnly (p <* optional "\n" <* endOfInput) i

solve :: Functor m => Parser a -> (a -> SolutionT m b) -> Solution' m a
solve p r = Solution p (void . r)

part1 :: (Show p, Monad m) => p -> SolutionT m ()
part1 m = _1 .= show m

part2 :: (Show p, Monad m) => p -> SolutionT m ()
part2 m = _2 .= show m
