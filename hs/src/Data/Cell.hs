{-# LANGUAGE TemplateHaskell #-}
module Data.Cell
  ( Cell(..)
  , col
  , row
  ) where

import Protolude

import Control.Lens    ((.=), at)
import Control.Lens.TH (makeLenses)

data Cell = Cell
  { _row :: Int
  , _col :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''Cell
