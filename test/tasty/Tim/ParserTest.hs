{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import RIO hiding (first)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Tim.Char (LowerChar (..), AlphaChar (..))
import Tim.Lexer (lex)
import Tim.Parser (parse)
import Tim.Parser.Types

nat :: Natural -> AST
nat = Rhs . RLit . Nat

int :: Int -> AST
int = Rhs . RLit . Int

float :: Double -> AST
float = Rhs . RLit . Float

stringS :: Text -> AST
stringS = Rhs . RLit . singleQuoted

stringD :: Text -> AST
stringD = Rhs . RLit . doubleQuoted

singleQuoted :: Text -> Literal
singleQuoted = String . SingleQuoted

doubleQuoted :: Text -> Literal
doubleQuoted = String . DoubleQuoted

list :: [Literal] -> AST
list = Rhs . RLit . List

dict :: Map StringLit Literal -> AST
dict = Rhs . RLit . Dict


type PrettyFailure = String

process :: Text -> Either PrettyFailure AST
process code =
  let x = parse =<< lex code
  in first (show . pretty) x

toBe :: Either PrettyFailure AST -> AST -> Assertion
actual `toBe` expected = actual @?= Right expected


test_literals :: [TestTree]
test_literals =
  [ testCase "42" testNats
  , testCase "+42, -42" testInts
  , testCase "'you'" testStrings
  , testCase "1.0" testFloats
  , testCase "v:true, v:false, v:null" testLiteralLikeVimVars
  , testCase "['sugar', 'sweet', 'moon']" testLists
  , testCase "{'foo': 10, 'bar': 20}" testDicts
  , testCase "ident" testIdents
  -- TODO
  -- , testCase "(foo)" testParens
  -- , testCase "function('string')"
  ]
  where
    testNats =
      process "42" `toBe` nat 42

    testInts = do
      process "+42" `toBe` int 42
      process "-42" `toBe` int (-42)

    testStrings = do
      process "'you'" `toBe` stringS "you"
      process "\"you\"" `toBe` stringD "you"

    testFloats =
      process "1.0" `toBe` float 1.0

    testLiteralLikeVimVars = do
      process "v:true"  `toBe` Rhs (RVar $ Scoped V "true")
      process "v:false" `toBe` Rhs (RVar $ Scoped V "false")
      process "v:null"  `toBe` Rhs (RVar $ Scoped V "null")

    testLists = do
      let expected = list [ singleQuoted "sugar"
                          , singleQuoted "sweet"
                          , singleQuoted "moon"
                          ]
      process "['sugar', 'sweet', 'moon']" `toBe` expected

    testDicts = do
      process "{'foo': 10, 'bar': 20}" `toBe`
        dict [ (SingleQuoted "foo", Nat 10)
             , (SingleQuoted "bar", Nat 20)
             ]
      process "{\"foo\": 10, 'bar': 20}" `toBe`
        dict [ (DoubleQuoted "foo", Nat 10)
             , (SingleQuoted "bar", Nat 20)
             ]
      process "{\"foo\": 10, \"bar\": 20}" `toBe`
        dict [ (DoubleQuoted "foo", Nat 10)
             , (DoubleQuoted "bar", Nat 20)
             ]

    testIdents = do
      process "simple" `toBe` Rhs (RVar $ SimpleLocal "simple")
      process "g:scoped" `toBe` Rhs (RVar $ Scoped G "scoped")
      process "@a" `toBe` Rhs (RVar . Register . Alphabetic $ AlphaLower A_)
      process "@+" `toBe` Rhs (RVar $ Register ClipboardPlus)

    -- testParens = do
    --   process "(10)" `toBe` Rhs (RParens ())
    --   process "(ident)" `toBe` Rhs (RParens)
    --   process "((nested))" `toBe` Rhs (RParens)
