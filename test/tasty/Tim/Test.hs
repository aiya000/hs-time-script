-- | Helpers for tests.
module Tim.Test where

import qualified Data.Text as Text
import RIO
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Tim.Main (process, PrettyFailure)
import Tim.Parser.Types
import Tim.String (Camel)
import qualified Tim.String.Parser as String

infixl 1 `shouldBe`

shouldBe :: HasCallStack => TestName -> AST -> TestTree
shouldBe expr expected =
  testCase expr $
    process (Text.pack expr) `toBe` expected
  where
    toBe :: HasCallStack => Either PrettyFailure AST -> AST -> Assertion
    actual `toBe` expected' = actual @?= Right expected'

syntax :: Syntax -> AST
syntax = Code . (: [])

-- | Unsafe
name :: String -> Camel
name = ignore . String.parseCamel
  where
    ignore (Left x)  = error x
    ignore (Right x) = x

-- | Unsafe
con :: String -> Type
con = Con . name
