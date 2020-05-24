{-# LANGUAGE TypeApplications #-}

module Tim.Lexer.Types.Combinators where

import Control.Lens ((+=), (.=))
import Control.Monad.State.Class (get, put)
import Data.Generics.Product (field)
import RIO
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Tim.Lexer.Types
import Tim.Processor (TokenPos)
-- import qualified Text.Megaparsec.Debug as P  -- Please see 'dbg'

-- |
-- Executes the taken lexer and increments 'colNum' by the taken `Int`,
-- and returns the lexer result with the token position.
forward :: Lexer a -> Int -> Lexer (a, TokenPos)
forward lexer len =
  forwardBy lexer $ const len

infixl 5 `forward`

-- |
-- Simular to 'forward',
-- but increments by taken function with the lexer result.
forwardBy :: Lexer a -> (a -> Int) -> Lexer (a, TokenPos)
forwardBy lexer makeLength = do
  pos <- get
  x <- lexer
  field @"colNum" += makeLength x
  pure (x, pos)

infixl 5 `forwardBy`

forward' :: Lexer a -> Int -> Lexer a
forward' lexer len =
  (fst <$>) . forwardBy lexer $ const len

infixl 5 `forward'`

forwardBy' :: Lexer a -> (a -> Int) -> Lexer a
forwardBy' lexer makeLength =
  fst <$> forwardBy lexer makeLength

infixl 5 `forwardBy'`

-- | Skips spaces of head and tail
token :: Lexer a -> Lexer a
token tokenLexer = do
  _ <- spaces
  result <- tokenLexer
  _ <- spaces
  pure result

-- | Rollbacks the state if taken lexer is failure
try' :: Lexer a -> Lexer a
try' lexer = do
  pos <- get
  P.try lexer <|> do  -- do if trying lexer failed
    put pos
    P.try lexer  -- To throw the same error, pull the error again


-- | Simular to P.spaceChar, but doesn't consume line-breaks.
spaceChar :: Lexer (Char, TokenPos)
spaceChar = (P.char ' ' <|> P.char '\t') `forward` 1

spaces :: Lexer [(Char, TokenPos)]
spaces = P.many spaceChar

lineBreak :: Lexer (Char, TokenPos)
lineBreak = down P.newline
  where
    -- Simular to 'forward',
    -- but increments 'lineNum' instead of 'colNum',
    -- and sets 'colNum' to 1.
    down :: Lexer a -> Lexer (a, TokenPos)
    down lexer = do
      pos <- get
      x <- lexer
      field @"colNum" .= 1
      field @"lineNum" += 1
      pure (x, pos)

(&>>) :: Lexer (a, TokenPos) -> b -> Lexer (b, TokenPos)
lexer &>> x = first (const x) <$> lexer

infixl 4 &>>


-- NOTE: Uncomment out this to debug.
-- dbg :: forall a. Show a => String -> Lexer a -> Lexer a
-- dbg name lexer = Lexer . P.dbg name $ unLexer lexer
dbg :: String -> Lexer a -> Lexer a
dbg _ lexer = lexer
