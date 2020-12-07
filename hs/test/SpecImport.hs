{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpecImport
  ( module X
  , fromRight
  , parseRight
  ) where

import Import       as X hiding (Selector, fromRight)
import Prelude      (error)
import Control.Monad.Fail (MonadFail(..))
import Test.Hspec   as X
import Text.Heredoc as X (here)

instance MonadFail (Either String) where
  fail = Left

-- A deliberately non-total version of fromRight, to allow for easier matching
-- in specs
fromRight :: Either a b -> b
fromRight = either (\_ -> error "Unexpected left") identity

parseRight :: Parser a -> Text -> a
parseRight parser input = either
  (\e -> error $ "Unexpected parse failure " <> e)
  identity
  $ parse parser input
