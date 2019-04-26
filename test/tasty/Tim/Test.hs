-- | Helpers for tests.
module Tim.Test where

import Data.Bifunctor (first)
import Data.Text.Prettyprint.Doc (pretty)
import RIO hiding (first)
import Test.Tasty.HUnit ((@?=), Assertion)
import Tim.Lexer (lex)
import Tim.Parser (parse)
import Tim.Parser.Types

type PrettyFailure = String

process :: Text -> Either PrettyFailure AST
process code =
  let x = parse =<< lex code
  in first (show . pretty) x

toBe :: HasCallStack => Either PrettyFailure AST -> AST -> Assertion
actual `toBe` expected = actual @?= Right expected
