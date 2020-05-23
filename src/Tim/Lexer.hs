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
import Tim.Processor (Failure, TokenPos)

-- | Tokenizes a code
lex :: Text -> Either Failure [(Token, TokenPos)]
lex = runLexer lexer . Text.unpack

lexer :: Lexer [(Token, TokenPos)]
lexer = P.many $ do
  _ <- P.many spaceChar `forwardBy` length
  result <- P.choice
              [ symbol
              , first Literal <$> literal
              , first Ident <$> ident
              ]
  _ <- P.space *> P.eof
  pure result

symbol :: Lexer (Token, TokenPos)
symbol = P.choice
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
  , lineBreak &>> LineBreak
  ]
  where
    -- Takes expected chars, and its corresponding token
    aSymbol :: String -> Token -> Lexer (Token, TokenPos)
    aSymbol expected itsToken =
      first (const itsToken) <$>
        token (P.string expected) `forwardBy` length

-- | Int literals
literal :: Lexer (AtomicLiteral, TokenPos)
literal = P.choice
  [ floatLiteral
  , natLiteral
  , intLiteral
  , stringLiteral
  ]

natLiteral :: Lexer (AtomicLiteral, TokenPos)
natLiteral =
  first Nat <$> try' P.decimal `forwardBy` length . show

intLiteral :: Lexer (AtomicLiteral, TokenPos)
intLiteral = try' $ first Int <$> int
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
floatLiteral =
  first Float <$> try' P.float `forwardBy` length . show

stringLiteral :: Lexer (AtomicLiteral, TokenPos)
stringLiteral = try' $ do
    (q, pos) <- quote `forward` 1  -- +1 is a length of `q`
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
ident =
  parseIdent `forwardBy` length . unIdent
