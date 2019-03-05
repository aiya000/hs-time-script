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

stringS :: Text -> AST
stringS = Literal . singleQuoted

stringD :: Text -> AST
stringD = Literal . doubleQuoted

singleQuoted :: Text -> Literal
singleQuoted = String . SingleQuoted

doubleQuoted :: Text -> Literal
doubleQuoted = String . DoubleQuoted

list :: [Literal] -> AST
list = Literal . List

dict :: Map StringLit Literal -> AST
dict = Literal . Dict

type PrettyFailure = String

process :: Text -> Either PrettyFailure AST
process code =
  let x = parse =<< lex code
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
      process "'you'" `toBe` stringS "you"
      process "\"you\"" `toBe` stringD "you"
  , testCase "1.0" $
      process "1.0" `toBe` float 1.0
  , testCase "v:true, v:false, v:null" $ do
      process "v:true" `toBe` VarIdent "v:true"
      process "v:false" `toBe` VarIdent "v:false"
      process "v:null" `toBe` VarIdent "v:null"
  , testCase "['sugar', 'sweet', 'moon']" testLists
  , testCase "{'foo': 10, 'bar': 20}" testDicts
  -- TODO
  --, testCase "function('string')"
  ]
  where
    testLists = do
      let expected = list [ singleQuoted "sugar"
                          , singleQuoted "sweet"
                          , singleQuoted "moon"
                          ]
      process "['sugar', 'sweet', 'moon']" `toBe` expected

    testDicts = do
      let expected = dict $ Map.fromList [ (SingleQuoted "foo", Nat 10)
                                         , (SingleQuoted "bar", Nat 20)
                                         ]
      process "{'foo': 10, 'bar': 20}" `toBe` expected
      let expected = dict $ Map.fromList [ (DoubleQuoted "foo", Nat 10)
                                         , (SingleQuoted "bar", Nat 20)
                                         ]
      process "{\"foo\": 10, 'bar': 20}" `toBe` expected
      let expected = dict $ Map.fromList [ (DoubleQuoted "foo", Nat 10)
                                         , (DoubleQuoted "bar", Nat 20)
                                         ]
      process "{\"foo\": 10, \"bar\": 20}" `toBe` expected
