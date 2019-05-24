{-# LANGUAGE QuasiQuotes #-}
module Tim.Util.String where

import Data.String.Here
import RIO
import RIO.List
import Tim.Util.List

-- |
-- Makes a comma separated message.
-- The last highlights with a word 'or'.
--
-- >>> enumerateByComma []
-- ""
--
-- >>> enumerateByComma ["xxx"]
-- "xxx"
--
-- >>> enumerateByComma ["xxx", "yyy"]
-- "xxx or yyy"
--
-- >>> enumerateByComma ["www", "xxx", "yyy", "zzz"]
-- "www, xxx, yyy, or zzz"
enumerateByComma :: [String] -> String
enumerateByComma [] = ""
enumerateByComma [x] = x
enumerateByComma [x, y] = [i|${x} or ${y}|]
enumerateByComma (x : y : z : rest) =
  case snocun rest of
    Just (last, body) -> [i|${x}, ${y}, ${comma body}, or ${last}|]
    Nothing -> [i|${x}, ${y}, or ${z}|]

-- |
-- Makes a comma separated message.
--
-- >>> comma ["a", "b", "c"]
-- "a, b, c"
comma :: [String] -> String
comma [] = ""
comma (x : xs) =
  let f a b = a <> ", " <> b
  in foldl f x xs
