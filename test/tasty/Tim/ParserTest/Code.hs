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
tName :: String -> Type
tName = flip Constr [] . name

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
          (Just $ Constr (name "Type") [])
          (RVar (SimpleLocal "y")))
      , "let x: (Type) = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just . Parens $ Constr (name "Type") [])
          (RVar (SimpleLocal "y")))
      , "let [x, y]: Type = z" `shouldBe` syntax  -- Time script doesn't allow the lhs `[x, y]` with non `Tuple X Y` types, but this is rejected by the syntax checker (not the parser).
        (Let
          (LDestuct [SimpleLocal "x", SimpleLocal "y"])
          (Just $ Constr (name "Type") [])
          (RVar (SimpleLocal "z")))
      ]

    testLetHigherTypes =
      [ "let x: X A = y" `shouldBe` syntax  -- simple
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ Constr (name "X") [tName "A"])
          (RVar (SimpleLocal "y")))
      , "let x: X A B = y" `shouldBe` syntax  -- two
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ Constr (name "X") [tName "A", tName "B"])
          (RVar (SimpleLocal "y")))
      , "let x: List (Tuple Char Nat) = y" `shouldBe` syntax  -- parens onto tail
        (Let
          (LVar $ SimpleLocal "x")
          (Just $
            Constr (name "List")
            [ Parens
              (Constr (name "Tuple")
                [ tName "Char"
                , tName "Nat"
                ])])
          (RVar (SimpleLocal "y")))
      ]

    testLetFunctionTypes =
      [ "let x: A -> B = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just $ tName "A" `Arrow` tName "B")
          (RVar (SimpleLocal "y")))
      , "let x: A -> B -> C = y" `shouldBe` syntax  -- right associated
        (Let
          (LVar $ SimpleLocal "x")
          (Just
            (Arrow
              (tName "A")
              (Arrow
                (tName "B")
                (tName "C"))))
          (RVar (SimpleLocal "y")))
      , "let x: List A -> Tuple A B = y" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "x")
          (Just $
            Constr (name "List") [tName "A"]
              `Arrow` Constr (name "Tuple") [tName "A", tName "B"])
          (RVar (SimpleLocal "y")))
      ]

    testLetUnionTypes =
      [ "let f: A | B = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $ tName "A" `Union` tName "B")
          (RVar (SimpleLocal "g")))
      , "let f: A -> B | X -> Y = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $ (tName "A" `Arrow` tName "B") `Union` (tName "X" `Arrow` tName "Y"))
          (RVar (SimpleLocal "g")))
      , "let f: List X | Tuple A B = g" `shouldBe` syntax
        (Let
          (LVar $ SimpleLocal "f")
          (Just $
            Constr (name "List") [tName "X"]
              `Union` Constr (name "Tuple") [tName "A", tName "B"])
          (RVar (SimpleLocal "g")))
      ]
