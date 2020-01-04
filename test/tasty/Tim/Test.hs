-- | Helpers for tests.
module Tim.Test where

import qualified Data.Text as Text
import RIO
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Tim.Main (process, PrettyFailure)
import Tim.Parser.Types

infixl 1 `shouldBe`

shouldBe :: HasCallStack => TestName -> AST -> TestTree
shouldBe name expected =
  testCase name $
    process (Text.pack name) `toBe` expected
  where
    toBe :: HasCallStack => Either PrettyFailure AST -> AST -> Assertion
    actual `toBe` expected' = actual @?= Right expected'
