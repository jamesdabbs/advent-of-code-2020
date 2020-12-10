module Main where

import Protolude
import Problems (problems)
import Santa (defaultMain)

main :: IO ()
main = defaultMain problems "../inputs"
