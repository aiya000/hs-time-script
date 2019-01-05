module Tim.ParserTest where

import Data.Text (Text)
import RIO
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Tim.Lexer (lex)
import Tim.Parser (parse)
import Tim.Parser.Types
import Tim.Processor (Failure)
import qualified Data.Map.Strict as Map

nat :: Natural -> AST
nat = Literal . Nat

int :: Int -> AST
int = Literal . Int

float :: Double -> AST
float = Literal . Float

string :: Text -> AST
string = Literal . String

list :: [Literal] -> AST
list = Literal . List

dict :: Map Text Literal -> AST
dict = Literal . Dict

process :: Text -> Either Failure AST
process = lex >=> parse

(@?>) :: Either Failure AST -> AST -> Assertion
actual @?> expected = actual @?= Right expected

test_literals :: [TestTree]
test_literals =
  [ testCase "42" $
      process "42" @?> nat 42
  , testCase "+42, -42" $ do
      process "+42" @?> int 42
      process "-42" @?> int (-42)
  , testCase "'you'" $ do
      process "'you'" @?> string "you"
      process "\"you\"" @?> string "you"
  , testCase "1.0" $
      process "1.0" @?> float 1.0
  , testCase "v:true, v:false, v:null" $ do
      process "v:true" @?> VarIdent "v:true"
      process "v:false" @?> VarIdent "v:false"
      process "v:null" @?> VarIdent "v:null"
  , testCase "['sugar', 'sweet', 'moon']" $ do
      let expected = list [String "sugar", String "sweet", String "moon"]
      process "['sugar', 'sweet', 'moon']" @?> expected
  , testCase "{'foo': 10, 'bar': 20}" $ do
      let expected = dict $ Map.fromList [ ("foo", Nat 10)
                                         , ("bar", Nat 20)
                                         ]
      process "{'foo': 10, 'bar': 20}" @?> expected
      process "{\"foo\": 10, \"bar\": 20}" @?> expected
  -- TODO
  --, testCase "function('string')"
  ]
