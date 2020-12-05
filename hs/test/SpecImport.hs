module SpecImport
  ( module X
  ) where

import Import       as X hiding (Selector, parse)
import Test.Hspec   as X
import Text.Heredoc as X (here)
