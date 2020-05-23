-- | Helpers for tests.
module Tim.Test where

import Data.String.Cases (Camel, parseCamel)
import Data.String.Here (i)
import qualified Data.Text as Text
import RIO
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit ((@?=), Assertion, testCase, assertFailure)
import Text.Megaparsec
import Tim.Main (process)
import Tim.Parser.Types hiding (String)

infixl 1 `shouldBe`

toBe :: (HasCallStack, Typeable e, Show e, Show a, Eq a) => Either e a -> a -> Assertion
(Right actual) `toBe` expected = actual @?= expected
(Left illegal) `toBe` _ =
  assertFailure $ trimMargins
    [i||  Expected a (Right _),
       |  but got: Left ${illegal}
    |]

toBeLeftOf :: (HasCallStack, Eq e, Show e, Typeable a, Show a) => Either e a -> e -> Assertion
(Left actual) `toBeLeftOf` expected = actual @?= expected
(Right illegal) `toBeLeftOf` _ =
  assertFailure $ trimMargins
    [i||  Expected a (Left _),
       |  but got: Right ${illegal}
    |]

thatShouldBe :: HasCallStack => TestName -> AST -> String -> TestTree
thatShouldBe testName expected expr =
  testCase testName $
    process (Text.pack expr) `toBe` expected

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


trimMargins :: String -> String
trimMargins = trim . unlines . map trim . lines
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
