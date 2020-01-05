{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.TypeCheckerTest where

import Data.String.Here (i)
import RIO
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)
import Tim.Parser.Types
import Tim.Test
import Tim.TypeChecker

hasType :: Rhs -> Type -> TestTree
hasType x t =
  testCase [i|`${x}` is typed by `${t}`|] .
    assertBool "howdy" . isValid $
      syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just t)
          x)

test_literals :: [TestTree]
test_literals =
  [ RLit (Nat 42) `hasType` con "Nat"
  , RLit (Int (-42)) `hasType` con "Int"
  , RLit (Float 10.0) `hasType` con "Float"
  , RLit (String $ SingleQuoted "000") `hasType` con "String"
  , RLit (String $ DoubleQuoted "1000") `hasType` con "String"
  , RLit (List [Nat 10]) `hasType` con "List Nat"
  , RLit (Dict [(SingleQuoted "x", Nat 10)]) `hasType` con "Dict Nat"
  ]

test_quasi_literals :: [TestTree]
test_quasi_literals =
  [
  ]
