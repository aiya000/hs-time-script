{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.ParserTest.Rhs where

import Data.Char.Cases hiding (G, S, V)
import Data.String.Cases
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
test_expressions = testIdents <> testParens
  where
    testIdents =
      [ "simple" `shouldBe` Rhs (RVar $ UnqualifiedVar [snakeQ|simple|])
      , "g:" `shouldBe` Rhs (RVar $ ScopedVar G "")
      , "g:scoped" `shouldBe` Rhs (RVar $ ScopedVar G "scoped")
      , "@a" `shouldBe` Rhs (RVar . RegisterVar . Alphabetic $ AlphaLower A_)
      , "@+" `shouldBe` Rhs (RVar $ RegisterVar ClipboardPlus)
      , "&opt" `shouldBe` Rhs (RVar . OptionVar $ UnscopedOption [lowerStringQ|opt|])
      , "&l:opt" `shouldBe` Rhs (RVar . OptionVar $ LocalScopedOption [lowerStringQ|opt|])
      , "&g:opt" `shouldBe` Rhs (RVar . OptionVar $ GlobalScopedOption [lowerStringQ|opt|])
      , "foo.bar" `shouldBe` Rhs (RVar . DictVar $
          PropertyAccessDictVar
            (UnqualifiedVarDictSelf [snakeQ|foo|])
            [snakeQ|bar|])
      , "s:foo.bar" `shouldBe` Rhs (RVar . DictVar $
          PropertyAccessDictVar
            (ScopedVarDictSelf S "foo")
            [snakeQ|bar|])
      , "g:.foo" `shouldBe` Rhs (RVar . DictVar $
          PropertyAccessDictVar
            (ScopedVarDictSelf G "")
            [snakeQ|foo|])
      , "foo[x]" `shouldBe` Rhs (RVar . DictVar $
          IndexAccessDictVar
            (UnqualifiedVarDictSelf [snakeQ|foo|])
            [snakeQ|x|])
      , "g:[x]" `shouldBe` Rhs (RVar . DictVar $
          IndexAccessDictVar
            (ScopedVarDictSelf G "")
            [snakeQ|x|])
      , "foo.bar.baz" `shouldBe` Rhs (RVar . DictVar $
          PropertyAccessDictVar
            (UnqualifiedVarDictSelf [snakeQ|foo|])
            [snakeQ|bar|] `PropertyAccessChainDictVar`
            [snakeQ|baz|])
      , "foo.bar[x]" `shouldBe` Rhs (RVar . DictVar $
          PropertyAccessDictVar
            (UnqualifiedVarDictSelf [snakeQ|foo|])
            [snakeQ|bar|] `IndexAccessChainDictVar`
            [snakeQ|x|])
      , "foo[x].bar" `shouldBe` Rhs (RVar . DictVar $
          IndexAccessDictVar
            (UnqualifiedVarDictSelf [snakeQ|foo|])
            [snakeQ|x|] `PropertyAccessChainDictVar`
            [snakeQ|bar|])
      ]

    testParens =
      [ "(10)" `shouldBe` Rhs (RParens . RLit $ Nat 10)
      , "(ident)" `shouldBe` Rhs (RParens . RVar $ UnqualifiedVar [snakeQ|ident|])
      , "((nested))" `shouldBe` Rhs (RParens . RParens . RVar $ UnqualifiedVar [snakeQ|nested|])
      ]
