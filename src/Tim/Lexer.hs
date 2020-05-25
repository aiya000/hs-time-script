{-# LANGUAGE TypeApplications #-}

module Tim.Lexer (lex, lexer, symbol, literal, ident) where

import Control.Lens ((+=))
import Control.Monad.State.Class (get)
import Data.Generics.Product (field)
import qualified Data.Text as Text
import RIO
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P hiding (space)
import Tim.Lexer.Types
import Tim.Lexer.Types.Combinators
import Tim.Processor (TokenPos)

-- | Tokenizes a code
lex :: Text -> Either LexErrorBundle [(Token, TokenPos)]
lex = runLexer lexer . Text.unpack

lexer :: Lexer [(Token, TokenPos)]
lexer = do
  _ <- dbg "beginning spaces" P.space
  result <- P.many once
  _ <- dbg "ending spaces and eof" $ P.space *> P.eof
  pure result
  where
    once = try' do
      _ <- dbg "spaces" $ P.many spaceChar `forwardBy` length
      dbg "lexer" $
        P.choice [ symbol
                 , first Literal <$> literal
                 , first Ident <$> ident
                 ]

symbol :: Lexer (Token, TokenPos)
symbol = dbg "symbol" $ P.choice
  [ aSymbol "=" Assign
  , aSymbol ":" Colon
  , aSymbol "," Comma
  , aSymbol "." Dot
  , aSymbol "#" Sharp
  , aSymbol "(" ParenBegin
  , aSymbol ")" ParenEnd
  , aSymbol "[" ListBegin
  , aSymbol "]" ListEnd
  , aSymbol "{" DictBegin
  , aSymbol "}" DictEnd
  , aSymbol "|" Bar
  , aSymbol "->" Arrow
  , newline &>> NewLine
  ]
  where
    -- Takes expected chars, and its corresponding token
    aSymbol :: String -> Token -> Lexer (Token, TokenPos)
    aSymbol expected itsToken =
      token (P.string expected) `forwardBy` length
        &>> itsToken

-- | Int literals
literal :: Lexer (AtomicLiteral, TokenPos)
literal = P.choice
  [ natLiteral
  , intLiteral
  , floatLiteral
  , stringLiteral
  ]

natLiteral :: Lexer (AtomicLiteral, TokenPos)
natLiteral = dbg "nat" . try' $ do
  (nat, pos) <- P.decimal `forwardBy` length . show
  _ <- P.notFollowedBy $ P.string "."  -- Avoid to parse floats
  pure (Nat nat, pos)

intLiteral :: Lexer (AtomicLiteral, TokenPos)
intLiteral = dbg "int" $ try' (first Int <$> int)
  where
    int :: Lexer (Int, TokenPos)
    int = do
      pos <- get
      s <- intSign
      nat <- P.decimal
      let signLen = 1
      let natLen = length $ show nat
      field @"colNum" += signLen + natLen
      pure (sign s nat, pos)

floatLiteral :: Lexer (AtomicLiteral, TokenPos)
floatLiteral = dbg "float" $
  first Float <$> try' (P.float `forwardBy` length . show)

stringLiteral :: Lexer (AtomicLiteral, TokenPos)
stringLiteral = dbg "string" $ try' $ do
    (q, pos) <- quote `forward` 1 -- +1 is a length of `q`
    (str, _) <- flip forwardBy ((+1) . length) $ P.manyTill P.charLiteral $ P.char (quoteToChar q)
    pure (String' q str, pos)

-- | Parses a string that surrounded by `'` or `"`
quote :: Lexer Quote
quote = P.choice
  [ P.char '\'' $> SingleQ
  , P.char '"' $> DoubleQ
  ]

data IntSign = IntPlus | IntMinus
  deriving (Show, Eq)

intSign :: Lexer IntSign
intSign = P.choice
  [ P.char '+' $> IntPlus
  , P.char '-' $> IntMinus
  ]

-- | Does sign
sign :: IntSign -> Natural -> Int
sign IntPlus nat = fromIntegral nat
sign IntMinus nat = negate $ fromIntegral nat

ident :: Lexer (Ident, TokenPos)
ident = dbg "ident" $
  parseIdent `forwardBy` length . unIdent
