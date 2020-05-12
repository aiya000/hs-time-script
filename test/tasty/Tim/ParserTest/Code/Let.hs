{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.ParserTest.Code.Let where

import Data.Char.Cases
import Data.String.Cases
import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Parser.Types
import Tim.Test

-- | Test '???' of 'let x: ??? = y'
typedBy :: Type -> AST
typedBy expected =
  syntax (Let
      (LhsVar $ VariableUnqualified [snakeQ|x|])
      (Just expected)
      (RhsVar $ VariableUnqualified [snakeQ|y|]))


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
            (LhsVar $ VariableUnqualified [snakeQ|x|])
            Nothing
            (RhsVar $ VariableUnqualified [snakeQ|y|]))
      , "let [x] = z" `shouldBe` syntax
          (Let
            (LhsDestuctVar [VariableUnqualified [snakeQ|x|]])
            Nothing
            (RhsVar $ VariableUnqualified [snakeQ|z|]))
      , "let [x, y] = z" `shouldBe` syntax
          (Let
            (LhsDestuctVar
              [ VariableUnqualified [snakeQ|x|]
              , VariableUnqualified [snakeQ|y|]
              ])
            Nothing
            (RhsVar $ VariableUnqualified [snakeQ|z|]))
      ]

    testLetAtomicTypes =
      [ "let x: Type = y" `shouldBe`
          typedBy (con "Type")
      , "let x: (Type) = y" `shouldBe`
          typedBy (con "Type")
      , "let [x, y]: Type = z" `shouldBe` syntax  -- Time script doesn't allow the lhs `[x, y]` with non `Tuple X Y` types, but it is rejected by the syntax checker (not the parser).
        (Let
          (LhsDestuctVar [VariableUnqualified [snakeQ|x|], VariableUnqualified [snakeQ|y|]])
          (Just $ con "Type")
          (RhsVar $ VariableUnqualified [snakeQ|z|]))
      ]

    testLetHigherTypes =
      [ "let x: X A = y" `shouldBe`
          typedBy
            (TypeApp
              (con "X")
              (con "A"))
      , "let x: X A B = y" `shouldBe`
          typedBy
            (TypeApp
              (TypeApp
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: (X) A B = y" `shouldBe`
          typedBy
            (TypeApp
              (TypeApp
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: X (A) B = y" `shouldBe`
          typedBy
            (TypeApp
              (TypeApp
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: X A (B) = y" `shouldBe`
          typedBy
            (TypeApp
              (TypeApp
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: (X A) B = y" `shouldBe`
          typedBy
            (TypeApp
              (TypeApp
                (con "X")
                (con "A"))
              (con "B"))
      , "let x: X (A B) = y" `shouldBe`
          typedBy
            (TypeApp
              (con "X")
              (TypeApp
                (con "A")
                (con "B")))
      , "let x: X A B C = y" `shouldBe`
          typedBy
            (TypeApp
              (TypeApp
                (TypeApp
                  (con "X")
                  (con "A"))
                (con "B"))
              (con "C"))
      ]

    testLetFunctionTypes =
      [ "let x: A -> B = y" `shouldBe`
          typedBy
            (TypeArrow
              (con "A")
              (con "B"))
      , "let x: A -> B -> C = y" `shouldBe`  -- right associated
          typedBy
            (TypeArrow
              (con "A")
              (TypeArrow
                (con "B")
                (con "C")))
      , "let x: X A -> Y A = y" `shouldBe`  -- TypeApp is stronger than TypeArrow
        typedBy
          (TypeArrow
            (TypeApp (con "X") (con "A"))
            (TypeApp (con "Y") (con "A")))
      ]

    testLetUnionTypes =
      [ "let x: A | B = y" `shouldBe`
          typedBy
            (TypeUnion
              (con "A")
              (con "B"))
      , "let x: A -> B | X -> Y = y" `shouldBe`  -- TypeArrow is stronger than TypeUnion
          typedBy
            (TypeUnion
              (TypeArrow (con "A") (con "B"))
              (TypeArrow (con "X") (con "Y")))
      , "let x: X A | Y A = y" `shouldBe`  -- TypeApp is stronger than TypeUnion
          typedBy
            (TypeUnion
              (TypeApp (con "X") (con "A"))
              (TypeApp (con "Y") (con "A")))
      ]
