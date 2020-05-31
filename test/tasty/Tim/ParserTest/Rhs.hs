{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.ParserTest.Rhs where

import Data.Char.Cases hiding (G, S, V)
import Data.String.Cases
import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Lexer.Types hiding (AtomicLiteral(..), QualifiedIdent(..))
import Tim.Parser.Types hiding (String)
import qualified Tim.Parser.Types as Parser
import Tim.Test

-- TODO
-- , testCase "function('string')"

test_nats :: [TestTree]
test_nats =
  [ "42" `shouldBe` Rhs (RhsLit $ LiteralNat 42)
  ]

test_ints :: [TestTree]
test_ints =
  [ "+42" `shouldBe` Rhs (RhsLit $ LiteralInt 42)
  , "-42" `shouldBe` Rhs (RhsLit $ LiteralInt (-42))
  ]

test_floats :: [TestTree]
test_floats =
  [ "1.0" `shouldBe` Rhs (RhsLit $ LiteralFloat 1.0)
  ]

test_strings :: [TestTree]
test_strings =
  [ "'you'" `shouldBe` Rhs (RhsLit . LiteralString $ StringLiteral "you")
  , "\"you\"" `shouldBe` Rhs (RhsLit . LiteralString $ StringDouble "you")
  ]

test_literal_like_vim_variables :: [TestTree]
test_literal_like_vim_variables =
  [ "v:true"  `shouldBe` Rhs (RhsVar . VariableScoped . ScopeVarV $ ScopedNameNonEmpty [snakeQ|true|])
  , "v:false" `shouldBe` Rhs (RhsVar . VariableScoped . ScopeVarV $ ScopedNameNonEmpty [snakeQ|false|])
  , "v:null"  `shouldBe` Rhs (RhsVar . VariableScoped . ScopeVarV $ ScopedNameNonEmpty [snakeQ|null|])
  ]

test_lists :: [TestTree]
test_lists =
  [
    let expected = Rhs . RhsLit $
          LiteralList [ LiteralString $ StringLiteral "sugar"
                      , LiteralString $ StringLiteral "sweet"
                      , LiteralString $ StringLiteral "moon"
                      ]
     in "['sugar', 'sweet', 'moon']" `shouldBe` expected
  ]

test_dicts :: [TestTree]
test_dicts =
  [ "{'foo': 10, 'bar': 20}" `shouldBe`
      dict [ (StringLiteral "foo", LiteralNat 10)
           , (StringLiteral "bar", LiteralNat 20)
           ]
  , "{\"foo\": 10, 'bar': 20}" `shouldBe`
      dict [ (StringDouble "foo", LiteralNat 10)
           , (StringLiteral "bar", LiteralNat 20)
           ]
  , "{\"foo\": 10, \"bar\": 20}" `shouldBe`
      dict [ (StringDouble "foo", LiteralNat 10)
           , (StringDouble "bar", LiteralNat 20)
           ]
  ]
   where
    dict :: Map Parser.String Literal -> AST
    dict = Rhs . RhsLit . LiteralDict


-- NOTE: Testing for FuncName is executed in Tim.ParserTest.Code.Function.
--       In here, tested only a one of FuncCalleeFuncName and FuncCalleeUnqualified.
test_function_call :: [TestTree]
test_function_call =
  [ "F()" `shouldBe` Rhs
      (RhsFuncCall
        (FuncCalleeFuncName $ FuncNameUnqualified [upperSnakeQ|F|])
        [])
  , "f()" `shouldBe` Rhs
      (RhsFuncCall
        (FuncCalleeUnqualified [snakeQ|f|])
        [])
  , "f(x)" `shouldBe` Rhs
      (RhsFuncCall
        (FuncCalleeUnqualified [snakeQ|f|])
        [RhsVar $ VariableUnqualified [snakeQ|x|]
        ])
  , "f(x, y)" `shouldBe` Rhs
      (RhsFuncCall
        (FuncCalleeUnqualified [snakeQ|f|])
        [ RhsVar $ VariableUnqualified [snakeQ|x|]
        ,  RhsVar $ VariableUnqualified [snakeQ|y|]
        ])
  , "f(x, y, )" `shouldBe` Rhs -- tail comma
      (RhsFuncCall
        (FuncCalleeUnqualified [snakeQ|f|])
        [ RhsVar $ VariableUnqualified [snakeQ|x|]
        ,  RhsVar $ VariableUnqualified [snakeQ|y|]
        ])
  , "f(g())" `shouldBe` Rhs
      (RhsFuncCall
        (FuncCalleeUnqualified [snakeQ|f|])
        [ RhsFuncCall
            (FuncCalleeUnqualified [snakeQ|g|])
            []
        ])
  ]


test_idents :: [TestTree]
test_idents =
  [ "simple" `shouldBe` Rhs
      (RhsVar $ VariableUnqualified [snakeQ|simple|])
  , "g:" `shouldBe` Rhs
      (RhsVar . VariableScoped $ ScopeVarG ScopedNameEmpty)
  , "g:scoped" `shouldBe` Rhs
      (RhsVar . VariableScoped . ScopeVarG $ ScopedNameNonEmpty [snakeQ|scoped|])
  , "a:000" `shouldBe` Rhs
      (RhsVar . VariableScoped $ ScopeVarA AScopeNameVarAll)
  , "a:1" `shouldBe` Rhs
      (RhsVar . VariableScoped . ScopeVarA $ AScopeNameVarNum 1)
  , "a:foo" `shouldBe` Rhs
      (RhsVar . VariableScoped . ScopeVarA . AScopeNameName $ ScopedNameNonEmpty [snakeQ|foo|])
  , "@a" `shouldBe` Rhs
      (RhsVar . VariableRegister . Alphabetic $ AlphaLower A_)
  , "@+" `shouldBe` Rhs
      (RhsVar $ VariableRegister ClipboardPlus)
  , "&opt" `shouldBe` Rhs
      (RhsVar . VariableOption $ UnscopedOption [lowerStringQ|opt|])
  , "&l:opt" `shouldBe` Rhs
      (RhsVar . VariableOption $ LocalScopedOption [lowerStringQ|opt|])
  , "&g:opt" `shouldBe` Rhs
      (RhsVar . VariableOption $ GlobalScopedOption [lowerStringQ|opt|])

  , "foo#bar" `shouldBe` Rhs (RhsVar . VariableAutoload $
        AutoloadVar ([snakeQ|foo|] :| []) (OmittableSnakeSnake [snakeQ|bar|]))
  , "foo#bar#baz" `shouldBe` Rhs (RhsVar . VariableAutoload $
        AutoloadVar ([snakeQ|foo|] :| [[snakeQ|bar|]])
          (OmittableSnakeSnake [snakeQ|baz|]))
  , "foo#" `shouldBe` Rhs (RhsVar . VariableAutoload $
        AutoloadVar ([snakeQ|foo|] :| []) OmittableSnakeOmitted)

  , "foo.bar" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarPropertyAccess
        (DictSelfUnqualified [snakeQ|foo|])
        [snakeQ|bar|])

  , "s:foo.bar" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarPropertyAccess
        (DictSelfScoped . ScopeVarS $ ScopedNameNonEmpty [snakeQ|foo|])
        [snakeQ|bar|])

  , "g:.foo" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarPropertyAccess
        (DictSelfScoped $ ScopeVarG ScopedNameEmpty)
        [snakeQ|foo|])

  , "foo[x]" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarIndexAccess
        (DictSelfUnqualified [snakeQ|foo|])
        (RhsVar $ VariableUnqualified [snakeQ|x|]))

  , "foo[s:x]" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarIndexAccess
        (DictSelfUnqualified [snakeQ|foo|])
        (RhsVar . VariableScoped . ScopeVarS $ ScopedNameNonEmpty [snakeQ|x|]))

  , "g:[x]" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarIndexAccess
        (DictSelfScoped $ ScopeVarG ScopedNameEmpty)
        (RhsVar $ VariableUnqualified [snakeQ|x|]))

  , "foo.bar.baz" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarPropertyAccess
        (DictSelfUnqualified [snakeQ|foo|])
        [snakeQ|bar|] `DictVarPropertyAccessChain`
        [snakeQ|baz|])

  , "foo.bar[x]" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarPropertyAccess
        (DictSelfUnqualified [snakeQ|foo|])
        [snakeQ|bar|] `DictVarIndexAccessChain`
        (RhsVar $ VariableUnqualified [snakeQ|x|]))

  , "foo[x].bar" `shouldBe` Rhs (RhsVar . VariableDict $
      DictVarIndexAccess
        (DictSelfUnqualified [snakeQ|foo|])
        (RhsVar $ VariableUnqualified [snakeQ|x|]) `DictVarPropertyAccessChain`
        [snakeQ|bar|])
  ]

test_parens :: [TestTree]
test_parens =
  [ "(10)" `shouldBe` Rhs (RhsParens . RhsLit $ LiteralNat 10)
  , "(ident)" `shouldBe` Rhs (RhsParens . RhsVar $ VariableUnqualified [snakeQ|ident|])
  , "((nested))" `shouldBe` Rhs (RhsParens . RhsParens . RhsVar $ VariableUnqualified [snakeQ|nested|])
  ]
