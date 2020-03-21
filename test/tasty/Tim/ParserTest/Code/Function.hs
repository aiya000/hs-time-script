{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.ParserTest.Code.Function where

import Data.Char.Cases
import Data.String.Cases
import Data.String.Here (i)
import RIO hiding (first)
import Test.Tasty (TestTree)
import Tim.Parser.Types
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
