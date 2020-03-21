{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest.Rhs where

import Data.Char.Cases (LowerChar (..), AlphaChar (..))
import RIO hiding (first)
import Test.Tasty (TestTree)
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
      [ "v:true"  `shouldBe` Rhs (RVar $ ScopedVar V "true")
      , "v:false" `shouldBe` Rhs (RVar $ ScopedVar V "false")
      , "v:null"  `shouldBe` Rhs (RVar $ ScopedVar V "null")
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
      [ "simple" `shouldBe` Rhs (RVar $ UnqualifiedVar "simple")
      , "g:scoped" `shouldBe` Rhs (RVar $ ScopedVar G "scoped")
      -- TODO
      -- , "foo.bar.baz"
      , "@a" `shouldBe` Rhs (RVar . RegisterVar . Alphabetic $ AlphaLower A_)
      , "@+" `shouldBe` Rhs (RVar $ RegisterVar ClipboardPlus)
      -- TODO
      -- , "&opt" `shouldBe` Rhs (RVar . OptionVar $ UnscopedOption 
      -- , "l:&opt" `shouldBe` Rhs (RVar . OptionVar 
      -- , "g:&opt" `shouldBe` Rhs (RVar . OptionVar 
      ]

    testParens =
      [ "(10)" `shouldBe` Rhs (RParens . RLit $ Nat 10)
      , "(ident)" `shouldBe` Rhs (RParens . RVar $ UnqualifiedVar "ident")
      , "((nested))" `shouldBe` Rhs (RParens . RParens . RVar $ UnqualifiedVar "nested")
      ]
