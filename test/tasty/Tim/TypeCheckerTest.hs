{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.TypeCheckerTest where

import Data.String.Here (i)
import RIO
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)
import Tim.Parser.Types
import Tim.Test
import qualified Tim.TypeChecker as TypeChecker

hasType :: Rhs -> Type -> TestTree
hasType x t =
  testCase [i|`${x}` is typed by `${t}`|] .
    assertBool "howdy" . TypeChecker.isValid $
      syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just t)
          x)

test_atomic_types :: [TestTree]
test_atomic_types =
  [ RLit (Nat 42) `hasType`
      con "Nat"
  , RLit (Int (-42)) `hasType`
      con "Int"
  , RLit (Float 10.0) `hasType`
      con "Float"
  , RLit (String $ SingleQuoted "000") `hasType`
      con "String"
  , RLit (String $ DoubleQuoted "1000") `hasType`
      con "String"
  ]

test_quasi_atomic_types :: [TestTree]
test_quasi_atomic_types =
  [ RLit (List [Nat 10]) `hasType`
      App (con "List") (con "Nat")
  , RLit (Dict [(SingleQuoted "x", Nat 10)]) `hasType`
      App (con "Dict") (con "Nat")
  ]

test_complex_types :: [TestTree]
test_complex_types =
  [ RLit (Nat 10) `hasType`
      Union (con "Nat") (con "String")
  , RLit (Nat 10) `hasType`
      Union (con "String") (con "Nat")
  -- TODO: Function types
  ]
