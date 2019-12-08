{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest.Code where

import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Parser.Types
import Tim.String
import qualified Tim.String.Parser as String
import Tim.Test

-- | A code
syntax :: Syntax -> AST
syntax = Code . (: [])

-- | Unsafe
name :: String -> Camel
name = ignore . String.parseCamel
  where
    ignore (Left x)  = error x
    ignore (Right x) = x

-- | Unsafe
con :: String -> Type
con = Con . name

-- | Test '???' of 'let x: ??? = y'
typedBy :: Type -> AST
typedBy expected =
  syntax (Let
      (LVar $ SimpleLocal "x")
      (Just expected)
      (RVar $ SimpleLocal "y"))

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
          (Just $ con "Type")
          (RVar (SimpleLocal "y")))
      , "let x: (Type) = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just . Parens $ con "Type")
          (RVar (SimpleLocal "y")))
      , "let [x, y]: Type = z" `shouldBe` syntax  -- Time script doesn't allow the lhs `[x, y]` with non `Tuple X Y` types, but this is rejected by the syntax checker (not the parser).
        (Let
          (LDestuct [SimpleLocal "x", SimpleLocal "y"])
          (Just $ con "Type")
          (RVar (SimpleLocal "z")))
      ]

    testLetHigherTypes =
      [ "let x: X A = y" `shouldBe`
          typedBy (App (con "X") [con "A"])
      , "let x: X A B = y" `shouldBe`
          typedBy (App (con "X") [con "A", con "B"])
      , "let x: (X) A B = y" `shouldBe`
          typedBy
            (App
              (Parens $ con "X")
              [con "A", con "B"])
      , "let x: X (A) B = y" `shouldBe`
          typedBy (App (con "X") [Parens $ con "A", con "B"])
      , "let x: X A (B) = y" `shouldBe`
          typedBy (App (con "X") [con "A", Parens $ con "B"])
      , "let x: (X A) B = y" `shouldBe`
          typedBy
            (App
              (App (con "X") [con "A"])
              [con "B"])
      , "let x: X (A B) = y" `shouldBe`
          typedBy
            (App (con "X")
                 [App (con "A")
                      [con "B"]])
      , "let x: (X A B) = y" `shouldBe`
          typedBy
            (Parens
              (App (con "X") [con "A", con "B"]))
      ]

    testLetFunctionTypes =
      [ "let x: A -> B = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ con "A" `Arrow` con "B")
          (RVar (SimpleLocal "y")))
      , "let x: A -> B -> C = y" `shouldBe` syntax  -- right associated
        (Let
          (LVar $ SimpleLocal "x")
          (Just
            (Arrow
              (con "A")
              (Arrow
                (con "B")
                (con "C"))))
          (RVar (SimpleLocal "y")))
      , "let x: List A -> Tuple A B = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just $
            App (con "List") [con "A"]
              `Arrow` App (con "Tuple") [con "A", con "B"])
          (RVar (SimpleLocal "y")))
      ]

    testLetUnionTypes =
      [ "let f: A | B = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $ con "A" `Union` con "B")
          (RVar (SimpleLocal "g")))
      , "let f: A -> B | X -> Y = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $ (con "A" `Arrow` con "B") `Union` (con "X" `Arrow` con "Y"))
          (RVar (SimpleLocal "g")))
      , "let f: List X | Tuple A B = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $
            App (con "List") [con "X"]
              `Union` App (con "Tuple") [con "A", con "B"])
          (RVar (SimpleLocal "g")))
      ]
