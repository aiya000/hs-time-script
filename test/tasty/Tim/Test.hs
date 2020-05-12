-- | Helpers for tests.
module Tim.Test where

import Data.String.Cases (Camel, parseCamel)
import qualified Data.Text as Text
import RIO
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Text.Megaparsec
import Tim.Main (process, PrettyFailure)
import Tim.Parser.Types hiding (String)

infixl 1 `shouldBe`

thatShouldBe :: HasCallStack => TestName -> AST -> String -> TestTree
thatShouldBe testName expected expr =
  testCase testName $
    process (Text.pack expr) `toBe` expected
  where
    toBe :: HasCallStack => Either PrettyFailure AST -> AST -> Assertion
    actual `toBe` expected' = actual @?= Right expected'

shouldBe :: HasCallStack => TestName -> AST -> TestTree
shouldBe expr expected = thatShouldBe expr expected expr

syntax :: Syntax -> AST
syntax = Code . (: [])

-- | Unsafe
name :: String -> Camel
name = ignore parseCamel
  where
    ignore parser input =
      case runParser parser "time-script test" input of
        Left  x -> error $ displayException x
        Right x -> x

-- | Unsafe
con :: String -> Type
con = TypeCon . name
