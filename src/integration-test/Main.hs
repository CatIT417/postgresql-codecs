module Main where

import Prelude
import IntegrationTest.Spec
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain spec
