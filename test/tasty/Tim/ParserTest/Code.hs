{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest.Code where

import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Parser.Types
import qualified Tim.String.Parser as String
import Tim.Test

-- | A code
syntax :: Syntax -> AST
syntax = Code . (: [])

-- | Unsafe
name :: String -> Type
name = Name . ignore . String.parseCamel
  where
    ignore (Left x)  = error x
    ignore (Right x) = x

test_let :: [TestTree]
test_let =
  testLet <>
  testLetAtomicTypes <>
  testLetHigherTypes <>
  testLetFunctionTypes <>
  testLetUnionTypes
  where
    testLet =
      [ "let x = y" `shouldBe` syntax
          (Let
            (LVar $ SimpleLocal "x")
            Nothing
            (RVar (SimpleLocal "y")))
      , "let [x] = z" `shouldBe` syntax
          (Let
            (LDestuct [SimpleLocal "x"])
            Nothing
            (RVar (SimpleLocal "z")))
      , "let [x, y] = z" `shouldBe` syntax
          (Let
            (LDestuct [SimpleLocal "x", SimpleLocal "y"])
            Nothing
            (RVar (SimpleLocal "z")))
      ]

    testLetAtomicTypes =
      [ "let x: Type = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ name "Type")
          (RVar (SimpleLocal "y")))
      , "let x: (Type) = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just . Parens $ name "Type")
          (RVar (SimpleLocal "y")))
      , "let [x, y]: Type = z" `shouldBe` syntax  -- Time script doesn't allow the lhs `[x, y]` with non `Tuple X Y` types, but this is rejected by the syntax checker (not the parser).
        (Let
          (LDestuct [SimpleLocal "x", SimpleLocal "y"])
          (Just $ name "Type")
          (RVar (SimpleLocal "z")))
      ]

    testLetHigherTypes =
      [ "let x: X A = y" `shouldBe` syntax  -- simple
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ name "X" `App` [name "A"])
          (RVar (SimpleLocal "y")))
      , "let x: X A B = y" `shouldBe` syntax  -- two
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ name "X" `App` [name "A", name "B"])
          (RVar (SimpleLocal "y")))
      , "let x: (X) A = y" `shouldBe` syntax  -- simple parens onto head
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ Parens (name "X") `App` [name "A"])
          (RVar (SimpleLocal "y")))
      , "let x: (X Y) A = y" `shouldBe` syntax  -- parens onto head
        (Let
          (LVar $ SimpleLocal "x")
          (Just $
            Parens (name "X" `App` [name "Y"]) `App`
              [name "A"])
          (RVar (SimpleLocal "y")))
      , "let x: List (Tuple Char Nat) = y" `shouldBe` syntax  -- parens onto tail
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ name "List" `App`
            [ Parens
              (name "Tuple" `App`
              [ name "Char"
              , name "Nat"
              ])
           ])
          (RVar (SimpleLocal "y")))
      ]

    testLetFunctionTypes =
      [ "let x: A -> B = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ name "A" `Arrow` name "B")
          (RVar (SimpleLocal "y")))
      , "let x: A -> B -> C = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ name "A" `Arrow` (name "B" `Arrow` name "C"))
          (RVar (SimpleLocal "y")))
      , "let x: List A -> Tuple A B = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just $
            name "List" `App` [name "A"]
              `Arrow` name "Tuple" `App` [name "A", name "B"])
          (RVar (SimpleLocal "y")))
      ]

    testLetUnionTypes =
      [ "let f: A | B = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $ name "A" `Union` name "B")
          (RVar (SimpleLocal "g")))
      , "let f: A -> B | X -> Y = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $ (name "A" `Arrow` name "B") `Union` (name "X" `Arrow` name "Y"))
          (RVar (SimpleLocal "g")))
      , "let f: List X | Tuple A B = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $
            name "List" `App` [name "X"]
              `Union` name "Tuple" `App` [name "A", name "B"])
          (RVar (SimpleLocal "g")))
      ]
