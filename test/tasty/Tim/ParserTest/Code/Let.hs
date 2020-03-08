{-# LANGUAGE OverloadedLists #-}

module Tim.ParserTest.Code.Let where

import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Parser.Types
import Tim.Test

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
      [ "let x: Type = y" `shouldBe`
          typedBy (con "Type")
      , "let x: (Type) = y" `shouldBe`
          typedBy (con "Type")
      , "let [x, y]: Type = z" `shouldBe` syntax  -- Time script doesn't allow the lhs `[x, y]` with non `Tuple X Y` types, but it is rejected by the syntax checker (not the parser).
        (Let
          (LDestuct [SimpleLocal "x", SimpleLocal "y"])
          (Just $ con "Type")
          (RVar (SimpleLocal "z")))
      ]

    testLetHigherTypes =
      [ "let x: X A = y" `shouldBe`
          typedBy
            (App
              (con "X")
              (con "A"))
      , "let x: X A B = y" `shouldBe`
          typedBy
            (App
              (App
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: (X) A B = y" `shouldBe`
          typedBy
            (App
              (App
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: X (A) B = y" `shouldBe`
          typedBy
            (App
              (App
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: X A (B) = y" `shouldBe`
          typedBy
            (App
              (App
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: (X A) B = y" `shouldBe`
          typedBy
            (App
              (App
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: X (A B) = y" `shouldBe`
          typedBy
            (App
              (con "X")
              (App
                (con "A")
                (con "B")))
      , "let x: X A B C = y" `shouldBe`
          typedBy
            (App
              (App
                (App
                  (con "X")
                  (con "A"))
                (con "B"))
              (con "C"))
      ]

    testLetFunctionTypes =
      [ "let x: A -> B = y" `shouldBe`
          typedBy
            (Arrow
              (con "A")
              (con "B"))
      , "let x: A -> B -> C = y" `shouldBe`  -- right associated
          typedBy
            (Arrow
              (con "A")
              (Arrow
                (con "B")
                (con "C")))
      , "let x: X A -> Y A = y" `shouldBe`  -- App is stronger than Arrow
        typedBy
          (Arrow
            (App (con "X") (con "A"))
            (App (con "Y") (con "A")))
      ]

    testLetUnionTypes =
      [ "let x: A | B = y" `shouldBe`
          typedBy
            (Union
              (con "A")
              (con "B"))
      , "let x: A -> B | X -> Y = y" `shouldBe`  -- Arrow is stronger than Union
          typedBy
            (Union
              (Arrow (con "A") (con "B"))
              (Arrow (con "X") (con "Y")))
      , "let x: X A | Y A = y" `shouldBe`  -- App is stronger than Union
          typedBy
            (Union
              (App (con "X") (con "A"))
              (App (con "Y") (con "A")))
      ]
