{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import GHC.Stack (HasCallStack)
import RIO hiding (first)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Tim.Char (LowerChar (..), AlphaChar (..))
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
  , testCase "(foo)" testParens
  -- TODO
  -- , testCase "function('string')"
  ]
  where
    testNats =
      process "42" `toBe` Rhs (RLit $ Nat 42)

    testInts = do
      process "+42" `toBe` Rhs (RLit $ Int 42)
      process "-42" `toBe` Rhs (RLit $ Int (-42))

    testFloats =
      process "1.0" `toBe` Rhs (RLit $ Float 1.0)

    testStrings = do
      process "'you'" `toBe` Rhs (RLit . String $ SingleQuoted "you")
      process "\"you\"" `toBe` Rhs (RLit . String $ DoubleQuoted "you")

    testLiteralLikeVimVars = do
      process "v:true"  `toBe` Rhs (RVar $ Scoped V "true")
      process "v:false" `toBe` Rhs (RVar $ Scoped V "false")
      process "v:null"  `toBe` Rhs (RVar $ Scoped V "null")

    testLists = do
      let expected = Rhs . RLit $
            List [ String $ SingleQuoted "sugar"
                 , String $ SingleQuoted "sweet"
                 , String $ SingleQuoted "moon"
                 ]
      process "['sugar', 'sweet', 'moon']" `toBe` expected

    dict :: Map StringLit Literal -> AST
    dict = Rhs . RLit . Dict

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

    testParens = do
      process "(10)" `toBe` Rhs (RParens . RLit $ Nat 10)
      process "(ident)" `toBe` Rhs (RParens . RVar $ SimpleLocal "ident")
      process "((nested))" `toBe` Rhs (RParens . RParens . RVar $ SimpleLocal "nested")
