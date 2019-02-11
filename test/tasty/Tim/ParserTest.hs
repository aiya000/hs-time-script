module Tim.ParserTest where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import RIO hiding (first)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Tim.Lexer (lex)
import Tim.Parser (parse)
import Tim.Parser.Types
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

type PrettyFailure = String

process :: Text -> Either PrettyFailure AST
process code =
  let x = parse <=< lex $ code
  in first (show . pretty) x

toBe :: Either PrettyFailure AST -> AST -> Assertion
actual `toBe` expected = actual @?= Right expected

test_literals :: [TestTree]
test_literals =
  [ testCase "42" $
      process "42" `toBe` nat 42
  , testCase "+42, -42" $ do
      process "+42" `toBe` int 42
      process "-42" `toBe` int (-42)
  , testCase "'you'" $ do
      process "'you'" `toBe` string "you"
      process "\"you\"" `toBe` string "you"
  , testCase "1.0" $
      process "1.0" `toBe` float 1.0
  , testCase "v:true, v:false, v:null" $ do
      process "v:true" `toBe` VarIdent "v:true"
      process "v:false" `toBe` VarIdent "v:false"
      process "v:null" `toBe` VarIdent "v:null"
  , testCase "['sugar', 'sweet', 'moon']" $ do
      let expected = list [String "sugar", String "sweet", String "moon"]
      process "['sugar', 'sweet', 'moon']" `toBe` expected
  , testCase "{'foo': 10, 'bar': 20}" $ do
      let expected = dict $ Map.fromList [ ("foo", Nat 10)
                                         , ("bar", Nat 20)
                                         ]
      process "{'foo': 10, 'bar': 20}" `toBe` expected
      process "{\"foo\": 10, \"bar\": 20}" `toBe` expected
  -- TODO
  --, testCase "function('string')"
  ]
