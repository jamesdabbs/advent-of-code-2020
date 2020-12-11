{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecImport
  ( module X,
    fromRight,
    parse,
    parseRight,
  )
where

import Control.Monad.Fail (MonadFail (..))
import qualified Data.Text as Text
import Import as X hiding (Selector, fromRight)
import qualified Santa.Solution as Santa (parse)
import Test.Hspec as X
import Text.Heredoc as X (here)
import Prelude (error)

instance MonadFail (Either String) where
  fail = Left

parse :: MonadFail m => Parser a -> Text -> m a
parse p i = case Santa.parse p i of
  Left err -> fail $ Text.unpack err
  Right val -> return val

-- A deliberately non-total version of fromRight, to allow for easier matching
-- in specs
fromRight :: Either a b -> b
fromRight = either (\_ -> error "Unexpected left") identity

parseRight :: Parser a -> Text -> a
parseRight parser input =
  either
    (\e -> error $ "Unexpected parse failure " <> e)
    identity
    $ parse parser input
