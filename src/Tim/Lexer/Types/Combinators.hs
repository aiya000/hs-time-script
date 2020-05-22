{-# LANGUAGE TypeApplications #-}

module Tim.Lexer.Types.Combinators where

import Control.Lens ((+=), (.=))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State.Class (get, put)
import Data.Generics.Product (field)
import RIO
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Tim.Lexer.Types
import Tim.Processor (TokenPos)

-- |
-- Executes the taken lexer and increments 'colNum' by the taken `Int`,
-- and returns the lexer result with the token position.
forward :: Lexer a -> Int -> Lexer (a, TokenPos)
forward lexer len =
  forwardBy lexer $ const len

infixl 5 `forward`

-- | Increments by taken function with the lexer result.
forwardBy :: Lexer a -> (a -> Int) -> Lexer (a, TokenPos)
forwardBy lexer makeLength = do
  pos <- get
  x <- lexer
  field @"colNum" += makeLength x
  pure (x, pos)

infixl 5 `forwardBy`

-- | Rollbacks the state if taken lexer is failure
restoreOnFail :: Lexer a -> Lexer a
restoreOnFail lexer = do
  pos <- get
  lexer `catchError` \e -> do
    put pos
    throwError e


lineBreak :: Lexer (Token, TokenPos)
lineBreak =
  first (const LineBreak) <$> down P.newline
  where
    down :: Lexer a -> Lexer (a, TokenPos)
    down lexer = do
      pos <- get
      x <- lexer
      field @"colNum" .= 1
      field @"lineNum" += 1
      pure (x, pos)

-- | Simular to P.spaceChar but doesn't take line-breaks.
spaces :: Lexer ()
spaces = void $
  (P.many $ P.char ' ' <|> P.char '\t') `forwardBy` length

-- | spaces or lineBreak
padding :: Lexer ()
padding = spaces <|> void lineBreak

-- | Simular to P.char, but increments state's colNum by 1.
char :: Char -> Lexer (Char, TokenPos)
char x = P.char x `forward` 1

-- | Simular to P.string, but increments state's colNum by the length of the string.
string :: String -> Lexer (String, TokenPos)
string x = P.string x `forwardBy` length

(&>>) :: Lexer (a, TokenPos) -> b -> Lexer (b, TokenPos)
lexer &>> result = lexer <&> first (const result)
