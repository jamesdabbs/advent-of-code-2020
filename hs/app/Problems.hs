{-# OPTIONS_GHC -F -pgmF app/problems.rb #-}

module Problems (run) where

import Protolude

{{IMPORTS}}

run :: Int -> Text -> IO ()
run n input = case n of
{{SOLUTIONS}}
  _ -> die $ "Not implemented: #" <> show n
