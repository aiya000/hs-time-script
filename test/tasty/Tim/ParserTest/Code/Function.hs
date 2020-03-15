{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.ParserTest.Code.Function where

import Data.String.Here (i)
import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Char
import Tim.Parser.Types
import Tim.String
import Tim.Test

test_function :: [TestTree]
test_function =
  [ ("simple" `thatShouldBe` syntax
      (Function (UnqualifiedFuncName [pascalQ|F|]) [] Nothing [] []))
      [i|
        function F()
        endfunction
      |]
  ]
