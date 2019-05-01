{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest.Code where

import RIO hiding (first)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Tim.Parser.Types
import Tim.Test
import qualified Tim.String.Parser as P

-- | A code
syntax :: Syntax -> AST
syntax = Code . (: [])

-- | Unsafe
name :: String -> Type
name = Name . ignore . P.parseCamel
  where
    ignore (Left x)  = error x
    ignore (Right x) = x

test_let :: [TestTree]
test_let =
  [ testCase "let x = y" testLet
  , testCase "let x: A = y" testLetAtomicTypes
  , testCase "let x: X A = y" testLetHigherKindTypes
  , testCase "let x: A -> B = y" testLetFunctionTypes
  , testCase "let x: A | B = y" testLetUnionTypes
  ]
  where
    testLet = do
      process "let x = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
         Nothing
         $ RVar (SimpleLocal "y"))
      process "let [x, y] = z" `toBe` syntax
        (Let (LDestuct [SimpleLocal "x", SimpleLocal "y"])
         Nothing
         $ RVar (SimpleLocal "y"))

    testLetAtomicTypes = do
      process "let x: Type = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ name "Type")
          $ RVar (SimpleLocal "y"))
      process "let x: (Type) = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just . Parens $ name "Type")
          $ RVar (SimpleLocal "y"))
      process "let [x, y]: Type = z" `toBe` syntax  -- Time script doesn't allow the lhs `[x, y]` with non `Tuple X Y` types, but this is rejected by the syntax checker (not the parser).
        (Let (LDestuct [SimpleLocal "x", SimpleLocal "y"])
          (Just $ name "Type")
          $ RVar (SimpleLocal "z"))

    testLetHigherKindTypes = do
      process "let x: X A = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ name "X" `App` [name "A"])
          $ RVar (SimpleLocal "y"))
      process "let x: (X) A = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ name "X" `App` [name "A"])
          $ RVar (SimpleLocal "y"))
      process "let x: X A B = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ name "X" `App` [name "A", name "B"])
          $ RVar (SimpleLocal "y"))
      process "let x: List (Tuple Char Nat) = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ name "List" `App` [
            name "Tuple" `App` [name "Char", name "Nat"]
          ])
          $ RVar (SimpleLocal "y"))

    testLetFunctionTypes = do
      process "let x: A -> B = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ name "A" `Arrow` name "B")
          $ RVar (SimpleLocal "y"))
      process "let x: A -> B -> C = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ name "A" `Arrow` (name "B" `Arrow` name "C"))
          $ RVar (SimpleLocal "y"))
      process "let x: List A -> Tuple A B = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $
            name "List" `App` [name "A"]
              `Arrow` name "Tuple" `App` [name "A", name "B"])
          $ RVar (SimpleLocal "y"))

    testLetUnionTypes = do
      process "let f: A | B = g" `toBe` syntax
        (Let (LVar $ SimpleLocal "f")
          (Just $ name "A" `Union` name "B")
          $ RVar (SimpleLocal "g"))
      process "let f: A -> B | X -> Y = g" `toBe` syntax
        (Let (LVar $ SimpleLocal "f")
          (Just $ (name "A" `Arrow` name "B") `Union` (name "X" `Arrow` name "Y"))
          $ RVar (SimpleLocal "g"))
      process "let f: List X | Tuple A B = g" `toBe` syntax
        (Let (LVar $ SimpleLocal "f")
          (Just $
            name "List" `App` [name "X"]
              `Union` name "Tuple" `App` [name "A", name "B"])
          $ RVar (SimpleLocal "g"))
