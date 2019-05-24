module Tim.Util.List where

import Data.List.NonEmpty (NonEmpty(..), init)
import RIO
import Safe

{-# ANN snocun ("HLint: Suggestion: Redundant $. Found: init $ x Why not: init x" :: String) #-}  -- TODO: hlint's parse error???
-- |
-- Tries to take a last and the rest, like `uncons`.
--
-- >>> snocun []
-- Nothing
--
-- >>> snocun [10]
-- Just (10, [])
--
-- >>> snocun [1..9]
-- Just (9, [1, 2, 3, 4, 5, 6, 7, 8])
snocun :: [a] -> Maybe (a, [a])
snocun [] = Nothing
snocun (x : xs) = (, init $ x :| xs) <$> lastMay xs
