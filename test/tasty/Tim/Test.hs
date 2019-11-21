-- | Helpers for tests.
module Tim.Test where

import Data.Bifunctor (first)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (pretty)
import RIO hiding (first)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Tim.Lexer (lex)
import Tim.Parser (parse)
import Tim.Parser.Types

type PrettyFailure = String

infixl 1 `shouldBe`

shouldBe :: HasCallStack => TestName -> AST -> TestTree
shouldBe name expected =
  testCase name $
    process (Text.pack name) `toBe` expected
  where
    process :: Text -> Either PrettyFailure AST
    process code =
      let x = parse =<< lex code
      in first (show . pretty) x

    toBe :: HasCallStack => Either PrettyFailure AST -> AST -> Assertion
    actual `toBe` expected' = actual @?= Right expected'
