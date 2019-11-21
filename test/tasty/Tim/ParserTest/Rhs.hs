{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest.Rhs where

import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Char (LowerChar (..), AlphaChar (..))
import Tim.Lexer.Types hiding (AtomicLiteral(..), QualifiedIdent(..))
import Tim.Parser.Types
import Tim.Test

test_literals :: [TestTree]
test_literals =
  testNats <>
  testInts <>
  testStrings <>
  testFloats <>
  testLiteralLikeVimVars <>
  testLists <>
  testDicts
  -- TODO
  -- , testCase "function('string')"
  where
    testNats =
      [ "42" `shouldBe` Rhs (RLit $ Nat 42)
      ]

    testInts =
      [ "+42" `shouldBe` Rhs (RLit $ Int 42)
      , "-42" `shouldBe` Rhs (RLit $ Int (-42))
      ]

    testFloats =
      [ "1.0" `shouldBe` Rhs (RLit $ Float 1.0)
      ]

    testStrings =
      [ "'you'" `shouldBe` Rhs (RLit . String $ SingleQuoted "you")
      , "\"you\"" `shouldBe` Rhs (RLit . String $ DoubleQuoted "you")
      ]

    testLiteralLikeVimVars =
      [ "v:true"  `shouldBe` Rhs (RVar $ Scoped V "true")
      , "v:false" `shouldBe` Rhs (RVar $ Scoped V "false")
      , "v:null"  `shouldBe` Rhs (RVar $ Scoped V "null")
      ]

    testLists =
      [
        let expected = Rhs . RLit $
              List [ String $ SingleQuoted "sugar"
                   , String $ SingleQuoted "sweet"
                   , String $ SingleQuoted "moon"
                   ]
         in "['sugar', 'sweet', 'moon']" `shouldBe` expected
      ]

    testDicts =
      [ "{'foo': 10, 'bar': 20}" `shouldBe`
          dict [ (SingleQuoted "foo", Nat 10)
               , (SingleQuoted "bar", Nat 20)
               ]
      , "{\"foo\": 10, 'bar': 20}" `shouldBe`
          dict [ (DoubleQuoted "foo", Nat 10)
               , (SingleQuoted "bar", Nat 20)
               ]
      , "{\"foo\": 10, \"bar\": 20}" `shouldBe`
          dict [ (DoubleQuoted "foo", Nat 10)
               , (DoubleQuoted "bar", Nat 20)
               ]
      ]
       where
        dict :: Map StringLit Literal -> AST
        dict = Rhs . RLit . Dict


-- | Non atomically expressions
test_expressions :: [TestTree]
test_expressions =
  testIdents <>
  testParens
  where
    testIdents =
      [ "simple" `shouldBe` Rhs (RVar $ SimpleLocal "simple")
      , "g:scoped" `shouldBe` Rhs (RVar $ Scoped G "scoped")
      , "@a" `shouldBe` Rhs (RVar . Register . Alphabetic $ AlphaLower A_)
      , "@+" `shouldBe` Rhs (RVar $ Register ClipboardPlus)
      ]

    testParens =
      [ "(10)" `shouldBe` Rhs (RParens . RLit $ Nat 10)
      , "(ident)" `shouldBe` Rhs (RParens . RVar $ SimpleLocal "ident")
      , "((nested))" `shouldBe` Rhs (RParens . RParens . RVar $ SimpleLocal "nested")
      ]
