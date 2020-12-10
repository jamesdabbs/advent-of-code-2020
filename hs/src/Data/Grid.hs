module Data.Grid
  ( Grid,
  )
where

import Protolude

type Grid a = [(Int, Int, a)]
