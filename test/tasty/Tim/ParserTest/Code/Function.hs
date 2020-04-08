{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.ParserTest.Code.Function where

import Data.Char.Cases hiding (UpperChar (G, S, B, W))
import Data.String.Cases
import Data.String.Here (i)
import RIO hiding (first)
import Test.Tasty (TestTree)
import qualified Tim.Lexer.Types as Scope
import Tim.Parser.Types
import Tim.Test

nameF :: FuncName
nameF = UnqualifiedFuncName [pascalQ|F|]

paramX :: FuncParam
paramX = UnboundFuncParam [snakeQ|x|]

paramY :: FuncParam
paramY = UnboundFuncParam [snakeQ|y|]

paramXNat :: FuncParam
paramXNat = BoundFuncParam [snakeQ|x|] $ Con [camelQ|Nat|]

paramYInt :: FuncParam
paramYInt = BoundFuncParam [snakeQ|y|] $ Con [camelQ|Int|]

letXWith10 :: Syntax
letXWith10 =
  Let
    (LVar $ UnqualifiedVar [snakeQ|x|])
    Nothing
    (RLit $ Nat 10)

typeVoid :: Type
typeVoid = Con [camelQ|Void|]

typeList :: Type
typeList = Con [camelQ|List|]

typeNat :: Type
typeNat = Con [camelQ|Nat|]

typeInt :: Type
typeInt = Con [camelQ|Int|]


test_function :: [TestTree]
test_function = names <> params <> ret <> syn <> opts
  where
    names =
      [ ("global" `thatShouldBe` syntax
          (Function nameF [] Nothing [] []))
          [i|
            function F()
            endfunction
          |]
      , ("scoped" `thatShouldBe` syntax
          (Function (ScopedFuncName Scope.S [snakeQ|f|]) [] Nothing [] []))
          [i|
            function s:f()
            endfunction
          |]
      , ("bound by a dict as a property" `thatShouldBe` syntax
          (Function
            (DictFuncName
              (PropertyAccessDictVar
                (UnqualifiedVarDictSelf [snakeQ|foo|]) [snakeQ|bar|]))
            [] Nothing [] []))
        [i|
          function foo.bar()
          endfunction
        |]
      , ("bound by a dict as an index" `thatShouldBe` syntax
          (Function
            (DictFuncName
              (PropertyAccessDictVar
                (UnqualifiedVarDictSelf [snakeQ|foo|]) [snakeQ|bar|]))
            [] Nothing [] []))
        [i|
          function foo['bar']()
          endfunction
        |]
      , ("autoload" `thatShouldBe` syntax
          (Function
            (AutoloadFuncName $
              [snakeQ|foo|] :| [[snakeQ|bar|], [snakeQ|baz|]])
            [] Nothing [] []))
          [i|
            function foo#bar#baz()
            endfunction
          |]
      ]

    params =
      [ ("with a parameter" `thatShouldBe` syntax
          (Function nameF [paramX] Nothing [] []))
          [i|
            function F(x)
            endfunction
          |]
      , ("with parameters" `thatShouldBe` syntax
          (Function nameF [paramX, paramY] Nothing [] []))
          [i|
            function F(x, y)
            endfunction
          |]
      , ("with typed parameters" `thatShouldBe` syntax
          (Function nameF [paramXNat, paramYInt] Nothing [] []))
          [i|
            function F(x: Nat, y: Int)
            endfunction
          |]
      , ("with variadic parameters" `thatShouldBe` syntax
          (Function nameF [VarFuncParams] Nothing [] []))
          [i|
            function F(...)
            endfunction
          |]
      , ("with params and variadic parameters" `thatShouldBe` syntax
          (Function nameF [paramX, VarFuncParams] Nothing [] []))
          [i|
            function F(x, ...)
            endfunction
          |]
      ]

    ret =
      [ ("with a return type" `thatShouldBe` syntax
          (Function nameF [] (Just typeVoid) [] []))
          [i|
            function F(): Void
            endfunction
          |]
      , ("with a higher kind return type" `thatShouldBe` syntax
          (Function nameF [] (Just $ typeList `App` typeNat) [] []))
          [i|
            function F(): List Nat
            endfunction
          |]
      , ("with a function return type" `thatShouldBe` syntax
          (Function nameF [] (Just $ typeNat `Arrow` typeInt) [] []))
          [i|
            function F(): Nat -> Int
            endfunction
          |]
      , ("with a union return type" `thatShouldBe` syntax
          (Function nameF [] (Just $ typeNat `Union` typeInt) [] []))
          [i|
            function F(): Nat | Int
            endfunction
          |]
      ]

    syn = []
      -- TODO
      -- [ ("with let and return" `thatShouldBe` syntax
      --     (Function nameF [] Nothing []
      --       [ letXWith10
      --       , Return . RVar $ UnqualifiedVar "x"
      --       ]
      --      ))
      --     [i|
      --       function F()
      --         let x = 10
      --         return x
      --       endfunction
      --     |]
      -- ]

    opts =
      [ ("with an option" `thatShouldBe` syntax
          (Function nameF [] Nothing [NoAbortFuncOpt] []))
          [i|
            function F() [[no-abort]]
            endfunction
          |]
      , ("with options" `thatShouldBe` syntax
          (Function nameF [] Nothing [NoAbortFuncOpt, NoClosureFuncOpt] []))
          [i|
            function F() [[no-abort]] [[no-closure]]
            endfunction
          |]
      ]
