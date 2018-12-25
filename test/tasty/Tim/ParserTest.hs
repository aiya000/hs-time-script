module Tim.ParserTest where

import Control.Monad ((<=<))
import Data.Text (Text)
import RIO
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=), Assertion)
import Tim.Lexer (lex)
import Tim.Parser (parse)
import Tim.Parser.Types
import Tim.Processor (runProcessor)
import qualified Data.Map.Strict as Map
import qualified Tim.Processor.Types as Proc

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

process :: _
process = runProcessor lex

(@?>) :: _
actual @?> expected = actual @?= Right expected

test_literals :: [TestTree]
test_literals =
  [ testCase "42" $
      parse "42" @?> nat 42
  , testCase "+42, -42" $ do
      parse "+42" @?> int +42
      parse "-42" @?> int -42
  , testCase "'you'" $ do
      parse "'you'" @?> string "you"
      parse "\"you\"" @?> string "you"
  , testCase "1.0" $
      parse "1.0" @?> float 1.0
  , testCase "v:true, v:false, v:null" $ do
      parse "v:true" @?> VarIdent "v:true"
      parse "v:false" @?> VarIdent "v:false"
      parse "v:null" @?> VarIdent "v:null"
  , testCase "['sugar', 'sweet', 'moon']" $ do
      let expected = list [string "sugar", string "sweet", string "moon"]
      parse "['sugar', 'sweet', 'moon']" @?> expected
  , testCase "{'foo': 10, 'bar': 20}" $ do
      let expected = dict $ Map.fromList [ ("foo", nat 10)
                                         , ("bar", nat 20)
                                         ]
      parse "{'foo': 10, 'bar': 20}" @?> expected
      parse "{\"foo\": 10, \"bar\": 20}" @?> expected
  -- TODO
  --, testCase "function('string')"
  ]
