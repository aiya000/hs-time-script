{-# LANGUAGE TypeApplications #-}

module Tim.Lexer.Types.Combinators where

import Control.Lens ((+=), (.=))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State.Class (get, put)
import Data.Generics.Product (field)
import RIO
import Text.Megaparsec (MonadParsec)
import Tim.Lexer.Types
import Tim.Processor (TokenPos)
import qualified Text.Megaparsec.Char as P

-- |
-- Executes the taken lexer and increments 'colNum' by the taken `Int`,
-- and returns the lexer result with the token position.
forward :: Lexer a -> Int -> Lexer (a, TokenPos)
forward lexer len =
  forwardVia lexer $ const len

infixl 5 `forward`

-- |
-- Simular to 'forward',
-- but increments by taken function with the lexer result.
forwardVia :: Lexer a -> (a -> Int) -> Lexer (a, TokenPos)
forwardVia lexer makeLength = do
  pos <- get
  x <- lexer
  field @"colNum" += makeLength x
  pure (x, pos)

infixl 5 `forwardVia`

forward' :: Lexer a -> Int -> Lexer a
forward' lexer len =
  (fst <$>) . forwardVia lexer $ const len

infixl 5 `forward'`

forwardVia' :: Lexer a -> (a -> Int) -> Lexer a
forwardVia' lexer makeLength =
  fst <$> forwardVia lexer makeLength

infixl 5 `forwardVia'`

-- |
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

-- | Skips spaces of head and tail
token :: MonadParsec e String m => m a -> m a
token tokenLexer = do
  _ <- P.space
  result <- tokenLexer
  _ <- P.space
  pure result

-- | Rollbacks the state if taken lexer is failure
restoreOnFail :: Lexer a -> Lexer a
restoreOnFail lexer = do
  pos <- get
  lexer `catchError` \e -> do
    put pos
    throwError e
