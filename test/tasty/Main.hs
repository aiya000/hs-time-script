-- {-# OPTIONS_GHC -F -pgmF tasty-discover #-}

module Main where

import Prelude
import Test.Tasty
-- import Tim.ParserTest.Code.Function
-- import Tim.ParserTest.Rhs
import Tim.ValidatorTest

main :: IO ()
main = defaultMain $ testGroup "selected"
  [ testGroup "" test_function_call
  ]
