{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.ParserTest.Code.Let where

import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Parser.Types
import Tim.Test

-- | Test '???' of 'let x: ??? = y'
typedBy :: Type -> AST
typedBy expected =
  syntax (Let
      (LhsVar $ VariableUnqualified "x")
      (Just expected)
      (RhsVar $ VariableUnqualified "y"))


test_let :: [TestTree]
test_let =
  [ "let x = y" `shouldBe` syntax
      (Let
        (LhsVar $ VariableUnqualified "x")
        Nothing
        (RhsVar $ VariableUnqualified "y"))
  , "let [x] = z" `shouldBe` syntax
      (Let
        (LhsDestuctVar [VariableUnqualified "x"])
        Nothing
        (RhsVar $ VariableUnqualified "z"))
  , "let [x, y] = z" `shouldBe` syntax
      (Let
        (LhsDestuctVar
          [ VariableUnqualified "x"
          , VariableUnqualified "y"
          ])
        Nothing
        (RhsVar $ VariableUnqualified "z"))
  ]

test_let_with_atomic_types :: [TestTree]
test_let_with_atomic_types =
  [ "let x: Type = y" `shouldBe`
      typedBy (TypeCon "Type")
  , "let x: (Type) = y" `shouldBe`
      typedBy (TypeCon "Type")
  , "let [x, y]: Type = z" `shouldBe` syntax  -- Time script doesn't allow the lhs `[x, y]` with non `Tuple X Y` types, but it is rejected by the syntax checker (not the parser).
    (Let
      (LhsDestuctVar [VariableUnqualified "x", VariableUnqualified "y"])
      (Just $ TypeCon "Type")
      (RhsVar $ VariableUnqualified "z"))
  ]

test_let_with_higher_types :: [TestTree]
test_let_with_higher_types =
  [ "let x: X A = y" `shouldBe`
      typedBy
        (TypeApp
          (TypeCon "X")
          (TypeCon "A"))
  , "let x: X A B = y" `shouldBe`
      typedBy
        (TypeApp
          (TypeApp
            (TypeCon "X")
            (TypeCon "A"))
          (TypeCon "B"))
  , "let x: (X) A B = y" `shouldBe`
      typedBy
        (TypeApp
          (TypeApp
            (TypeCon "X")
            (TypeCon "A"))
          (TypeCon "B"))
  , "let x: X (A) B = y" `shouldBe`
      typedBy
        (TypeApp
          (TypeApp
            (TypeCon "X")
            (TypeCon "A"))
          (TypeCon "B"))
  , "let x: X A (B) = y" `shouldBe`
      typedBy
        (TypeApp
          (TypeApp
            (TypeCon "X")
            (TypeCon "A"))
          (TypeCon "B"))
  , "let x: (X A) B = y" `shouldBe`
      typedBy
        (TypeApp
          (TypeApp
            (TypeCon "X")
            (TypeCon "A"))
          (TypeCon "B"))
  , "let x: X (A B) = y" `shouldBe`
      typedBy
        (TypeApp
          (TypeCon "X")
          (TypeApp
            (TypeCon "A")
            (TypeCon "B")))
  , "let x: X A B C = y" `shouldBe`
      typedBy
        (TypeApp
          (TypeApp
            (TypeApp
              (TypeCon "X")
              (TypeCon "A"))
            (TypeCon "B"))
          (TypeCon "C"))
  ]

test_let_with_function_types :: [TestTree]
test_let_with_function_types =
  [ "let x: A -> B = y" `shouldBe`
      typedBy
        (TypeArrow
          (TypeCon "A")
          (TypeCon "B"))
  , "let x: A -> B -> C = y" `shouldBe`  -- right associated
      typedBy
        (TypeArrow
          (TypeCon "A")
          (TypeArrow
            (TypeCon "B")
            (TypeCon "C")))
  , "let x: X A -> Y A = y" `shouldBe`  -- TypeApp is stronger than TypeArrow
    typedBy
      (TypeArrow
        (TypeApp (TypeCon "X") (TypeCon "A"))
        (TypeApp (TypeCon "Y") (TypeCon "A")))
  ]

test_let_with_union_types :: [TestTree]
test_let_with_union_types =
  [ "let x: A | B = y" `shouldBe`
      typedBy
        (TypeUnion
          (TypeCon "A")
          (TypeCon "B"))
  , "let x: A -> B | X -> Y = y" `shouldBe`  -- TypeArrow is stronger than TypeUnion
      typedBy
        (TypeUnion
          (TypeArrow (TypeCon "A") (TypeCon "B"))
          (TypeArrow (TypeCon "X") (TypeCon "Y")))
  , "let x: X A | Y A = y" `shouldBe`  -- TypeApp is stronger than TypeUnion
      typedBy
        (TypeUnion
          (TypeApp (TypeCon "X") (TypeCon "A"))
          (TypeApp (TypeCon "Y") (TypeCon "A")))
  ]
