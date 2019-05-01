{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest.Code where

import RIO hiding (first)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, Assertion)
import Tim.Char (LowerChar (..), AlphaChar (..))
import Tim.Parser.Types
import Tim.Test

-- | A code
syntax :: Syntax -> AST
syntax = Code . (: [])


test_let :: [TestTree]
test_let =
  [ testCase "let x = y" testLet
  , testCase "let x: A = y" testLetAtomicTypes
  , testCase "let x: X A = y" testLetHigherKindTypes
  , testCase "let x: A -> B = y" testLetFunctionTypes
  , testCase "let x: A | B = y" testLetUnionTypes
  ]
  where
    testLet =
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
          (Just $ Lower "Type")
          $ RVar (SimpleLocal "y"))
      process "let x: (Type) = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just . TParens $ Lower "Type")
          $ RVar (SimpleLocal "y"))
      process "let [x, y]: Type = z" `toBe` syntax  -- Time script doesn't allow the lhs `[x, y]` with non `Tuple X Y` types, but this is rejected by the syntax checker (not the parser).
        (Let (LDestuct [SimpleLocal "x", SimpleLocal "y"])
          (Just $ Lower "Type")
          $ RVar (SimpleLocal "z"))

    testLetHigherKindTypes = do
      process "let x: X A = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ Higher "X" [Lower "A"])
          $ RVar (SimpleLocal "y"))
      process "let x: X A B = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ Higher "X" [Lower "A", Lower "B"])
          $ RVar (SimpleLocal "y"))
      process "let x: List (Tuple Char Nat) = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ Higher "List" [Higher "Tuple" [Lowre "Char", Lower "Nat"]])
          $ RVar (SimpleLocal "y"))

    testLetFunctionTypes = do
      process "let x: A -> B = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ Lower "A" `Arrow` Lower "B")
          $ RVar (SimpleLocal "y"))
      process "let x: A -> B -> C = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ Lower "A" `Arrow` (Lower "B" `Arrow` Lower "C"))
          $ RVar (SimpleLocal "y"))
      process "let x: List A -> Tuple A B = y" `toBe` syntax
        (Let (LVar $ SimpleLocal "x")
          (Just $ Higher "List" [Lower "A"] `Arrow` Higher "Tuple" [Lower "A", Lower "B"])
          $ RVar (SimpleLocal "y"))

    testLetUnionTypes = do
      process "let f: A | B = g" `toBe` syntax
        (Let (LVar $ SimpleLocal "f")
          (Just $ Lower "A" `Union` Lower "B")
          $ RVar (SimpleLocal "g"))
      process "let f: A -> B | X -> Y = g" `toBe` syntax
        (Let (LVar $ SimpleLocal "f")
          (Just $ (Lower "A" `Arrow` Lower "B") `Union` (Lower "X" `Arrow` Lower "Y"))
          $ RVar (SimpleLocal "g"))
      process "let f: List X | Tuple A B = g" `toBe` syntax
        (Let (LVar $ SimpleLocal "f")
          (Just $ Higher "List" [Lower "X"] `Union` Higher "Tuple" [Lower "A", Lower "B"])
          $ RVar (SimpleLocal "g"))
