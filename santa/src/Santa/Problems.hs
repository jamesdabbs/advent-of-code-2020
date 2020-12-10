module Santa.Problems
  ( Problems
  ) where

import Protolude
import Santa.Solution (Error)

type Problems = Map Text (Text -> Either Error (Text, Text))
