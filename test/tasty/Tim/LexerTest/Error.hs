module Tim.LexerTest.Error where

import RIO hiding (first)
import Test.Tasty
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Tim.Lexer
import Tim.Processor

shouldBeErrorOn :: Show a => Either Failure a -> TokenPos -> Assertion
shouldBeErrorOn (Left (Failure _ (OnAToken actual))) expected =
  actual @?= expected
shouldBeErrorOn x _ =
  error $ "Test error! A passed Failure doesn't have a TokenPos: " <> show x


test_lexer_error_positions :: [TestTree]
test_lexer_error_positions =
  [ testCase "columns" $
      lex "let x = ???" `shouldBeErrorOn` TokenPos 1 9
  , testCase "lines" $
      lex "function F()\nlet x = ???\nendfunction" `shouldBeErrorOn` TokenPos 2 9
  ]
