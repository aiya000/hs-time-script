{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.ParserTest.Code.Function where

import Data.Char.Cases hiding (UpperChar (G, S, B, W))
import Data.String.Cases
import Data.String.Here (i)
import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Parser.Types
import Tim.Test

nameF :: FuncName
nameF = FuncNameUnqualified [snakeQ|F|]

paramX :: FuncParam
paramX = FuncParamUnbound [snakeQ|x|]

paramY :: FuncParam
paramY = FuncParamUnbound [snakeQ|y|]

paramXNat :: FuncParam
paramXNat = FuncParamBound [snakeQ|x|] $ TypeCon [camelQ|Nat|]

paramYInt :: FuncParam
paramYInt = FuncParamBound [snakeQ|y|] $ TypeCon [camelQ|Int|]

letXWith10 :: Syntax
letXWith10 =
  Let
    (LhsVar $ VariableUnqualified [snakeQ|x|])
    Nothing
    (RhsLit $ LiteralNat 10)

typeVoid :: Type
typeVoid = TypeCon [camelQ|Void|]

typeList :: Type
typeList = TypeCon [camelQ|List|]

typeNat :: Type
typeNat = TypeCon [camelQ|Nat|]

typeInt :: Type
typeInt = TypeCon [camelQ|Int|]


test_function_names :: [TestTree]
test_function_names =
  [ ("global" `thatShouldBe` syntax
      (Function nameF [] Nothing [] []))
      [i|
        function F()
        endfunction
      |]
  , ("scoped" `thatShouldBe` syntax
      (Function (FuncNameScoped . ScopeVarS $ ScopedNameNonEmpty [snakeQ|f|]) [] Nothing [] []))
      [i|
        function s:f()
        endfunction
      |]
  , ("bound by a dict as a property" `thatShouldBe` syntax
      (Function
        (FuncNameDict
          (DictVarPropertyAccess
            (DictSelfUnqualified [snakeQ|foo|]) [snakeQ|bar|]))
        [] Nothing [] []))
    [i|
      function foo.bar()
      endfunction
    |]
  , ("bound by a dict as an index" `thatShouldBe` syntax
      (Function
        (FuncNameDict
          (DictVarIndexAccess
            (DictSelfUnqualified [snakeQ|foo|])
            (RhsLit . LiteralString $ StringLiteral "bar")))
        [] Nothing [] []))
    [i|
      function foo['bar']()
      endfunction
    |]
  , ("autoload" `thatShouldBe` syntax
      (Function
        (FuncNameAutoload $
          AutoloadVar ([snakeQ|foo|] :| [[snakeQ|bar|]]) (OmittableSnakeSnake [snakeQ|baz|]))
        [] Nothing [] []))
      [i|
        function foo#bar#baz()
        endfunction
      |]
  ]

test_function_params :: [TestTree]
test_function_params =
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
      (Function nameF [FuncParamVariadic] Nothing [] []))
      [i|
        function F(...)
        endfunction
      |]
  , ("with params and variadic parameters" `thatShouldBe` syntax
      (Function nameF [paramX, FuncParamVariadic] Nothing [] []))
      [i|
        function F(x, ...)
        endfunction
      |]
  ]

test_function_with_return_types :: [TestTree]
test_function_with_return_types =
  [ ("with a return type" `thatShouldBe` syntax
      (Function nameF [] (Just typeVoid) [] []))
      [i|
        function F(): Void
        endfunction
      |]
  , ("with a higher kind return type" `thatShouldBe` syntax
      (Function nameF [] (Just $ typeList `TypeApp` typeNat) [] []))
      [i|
        function F(): List Nat
        endfunction
      |]
  , ("with a function return type" `thatShouldBe` syntax
      (Function nameF [] (Just $ typeNat `TypeArrow` typeInt) [] []))
      [i|
        function F(): Nat -> Int
        endfunction
      |]
  , ("with a union return type" `thatShouldBe` syntax
      (Function nameF [] (Just $ typeNat `TypeUnion` typeInt) [] []))
      [i|
        function F(): Nat | Int
        endfunction
      |]
  ]

test_function_with_syntaxes :: [TestTree]
test_function_with_syntaxes = []
  -- TODO
  -- [ ("with let and return" `thatShouldBe` syntax
  --     (Function nameF [] Nothing []
  --       [ letXWith10
  --       , Return . RVar $ VariableUnqualified "x"
  --       ]
  --      ))
  --     [i|
  --       function F()
  --         let x = 10
  --         return x
  --       endfunction
  --     |]
  -- ]

test_function_options :: [TestTree]
test_function_options =
  [ ("with an option" `thatShouldBe` syntax
      (Function nameF [] Nothing [FuncOptNoAbort] []))
      [i|
        function F() [[no-abort]]
        endfunction
      |]
  , ("with options" `thatShouldBe` syntax
      (Function nameF [] Nothing [FuncOptNoAbort, FuncOptNoClosure] []))
      [i|
        function F() [[no-abort]] [[no-closure]]
        endfunction
      |]
  ]
