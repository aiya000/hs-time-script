{-# LANGUAGE TypeApplications #-}

module Tim.Lexer (lex) where

import Control.Lens ((+=))
import Control.Monad.State.Class (get)
import Data.Generics.Product (field)
import Data.Text (Text)
import Numeric.Natural (Natural)
import RIO
import Tim.Lexer.Types
import Tim.Lexer.Types.Combinators
import Tim.Lexer.Types.Idents
import Tim.Processor (Failure, TokenPos)
import Tim.String
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P hiding (space)

-- | Tokenizes a code
lex :: Text -> Either Failure [(Token, TokenPos)]
lex = runLexer rex . Text.unpack

rex :: Lexer [(Token, TokenPos)]
rex = P.many $ do
  _ <- P.many P.spaceChar `forwardVia` length
  symbol <|> first Literal <$> literal <|>
             first Var <$> varIdent <|>
             first Type <$> typeIdent <|>
             first Command <$> cmdIdent

symbol :: Lexer (Token, TokenPos)
symbol =
  aSymbol '=' Assign <|>
  aSymbol ':' Colon <|>
  aSymbol ',' Comma <|>
  aSymbol '(' ParenBegin <|>
  aSymbol ')' ParenEnd <|>
  aSymbol '[' ListBegin <|>
  aSymbol ']' ListEnd <|>
  aSymbol '{' DictBegin <|>
  aSymbol '}' DictEnd <|>
  lineBreak
  where
    -- Takes expected chars, and its correspound token
    aSymbol :: Char -> Token -> Lexer (Token, TokenPos)
    aSymbol expected itsToken =
      first (const itsToken) <$>
        token (P.char expected) `forward` 1

    lineBreak :: Lexer (Token, TokenPos)
    lineBreak =
      first (const LineBreak) <$>
        down P.newline

-- | Int literals
literal :: Lexer (AtomicLiteral, TokenPos)
literal =
  floatLiteral <|>
  natLiteral <|>
  intLiteral <|>
  stringLiteral

natLiteral :: Lexer (AtomicLiteral, TokenPos)
natLiteral =
  first Nat <$> P.try P.decimal `forwardVia` length . show

intLiteral :: Lexer (AtomicLiteral, TokenPos)
intLiteral = restoreOnFail $
  first Int <$> int
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
  first Float <$> P.try P.float `forwardVia` length . show

stringLiteral :: Lexer (AtomicLiteral, TokenPos)
stringLiteral = P.try $ do
    (q, pos) <- quote `forward` 1
    -- +1 is a length of `q`
    (str, _) <- flip forwardVia ((+1) . length) $ P.manyTill P.charLiteral $ P.char (toChar q)
    pure (String' q str, pos)

-- | Parses a string that surrounded by `'` or `"`
quote :: Lexer Quote
quote =
  P.char '\'' $> SingleQ <|>
  P.char '"' $> DoubleQ

data IntSign = IntPlus | IntMinus
  deriving (Show, Eq)

intSign :: Lexer IntSign
intSign = plus <|> minus
  where
    plus = P.char '+' $> IntPlus
    minus = P.char '-' $> IntMinus

-- | Does sign
sign :: IntSign -> Natural -> Int
sign IntPlus nat = fromIntegral nat
sign IntMinus nat = negate $ fromIntegral nat

varIdent :: Lexer (VarIdent, TokenPos)
varIdent =
  parseNonEmpty `forwardVia` length . unNonEmpty

typeIdent :: Lexer (TypeIdent, TokenPos)
typeIdent =
  parsePascal `forwardVia` length . unPascal

cmdIdent :: Lexer (CmdIdent, TokenPos)
cmdIdent =
  parseCamel `forwardVia` length . unCamel
