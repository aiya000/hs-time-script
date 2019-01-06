{-# LANGUAGE TypeApplications #-}

module Tim.Lexer  where

import Control.Lens ((+=), (.=))
import Control.Monad.State.Class (get)
import Data.Generics.Product (field)
import Data.Text (Text)
import Numeric.Natural (Natural)
import RIO
import Text.Megaparsec (MonadParsec)
import Tim.Lexer.Types
import Tim.Lexer.Types.Idents
import Tim.Processor (Failure, TokenPos)
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P hiding (space)

-- | Tokenizes a code
lex :: Text -> Either Failure [(Token, TokenPos)]
lex = runLexer rex . Text.unpack

rex :: Lexer [(Token, TokenPos)]
rex = P.many $
  symbol <|>
  first Literal <$> literal <|>
  first Var <$> varIdent <|>
  first Type <$> typeIdent <|>
  first Command <$> cmdIdent

-- | Skips spaces of head and tail
token :: MonadParsec e String m => m a -> m a
token tokenParser = do
  _ <- P.space
  result <- tokenParser
  _ <- P.space
  pure result

symbol :: Lexer (Token, TokenPos)
symbol =
  forward '=' Assign <|>
  forward ':' Colon <|>
  forward ',' Comma <|>
  forward '(' ParenBegin <|>
  forward ')' ParenEnd <|>
  forward '[' ListBegin <|>
  forward ']' ListEnd <|>
  forward '{' DictBegin <|>
  forward '}' DictEnd <|>
  down
  where
    charToken :: Char -> Lexer Char
    charToken = token . P.char

    -- Takes expected chars, and its correspound token
    forward :: Char -> Token -> Lexer (Token, TokenPos)
    forward expected itsToken = do
      result <- (itsToken,) <$> get
      _ <- charToken expected
      field @"colNum" += 1
      pure result

    down :: Lexer (Token, TokenPos)
    down = do
      current <- (LineBreak,) <$> get
      _ <- P.newline
      field @"colNum" .= 1
      field @"lineNum" += 1
      pure current

-- | Int literals
literal :: Lexer (AtomicLiteral, TokenPos)
literal =
  natLiteral <|>
  intLiteral <|>
  floatLiteral <|>
  stringLiteral
  where
    natLiteral = do
      pos <- get
      nat <-P.decimal
      field @"colNum" += length (show nat)
      pure (Nat nat, pos)

    intLiteral = do
      pos <- get
      int <- sign <$> intSign <*> P.decimal
      field @"colNum" += length (show int)
      pure (Int int, pos)

    floatLiteral = do
      pos <- get
      float <- P.float
      field @"colNum" += length (show float)
      pure (Float float, pos)

    stringLiteral = doubleQuoted <|> singleQuoted

    doubleQuoted = do
      pos <- get
      _ <- P.char '"'
      str <- P.manyTill P.charLiteral $ P.char '"'
      pure (String' str, pos)

    singleQuoted = do
      pos <- get
      _ <- P.char '\''
      str <- P.manyTill P.charLiteral $ P.char '\''
      pure (String' str, pos)

data IntSign = IntPlus | IntMinus
  deriving (Show, Eq)

intSign :: Lexer IntSign
intSign = plus <|> minus
  where
    plus = P.char '+' $> IntPlus
    minus = P.char '-' $> IntMinus

sign :: IntSign -> Natural -> Int
sign IntPlus nat = fromIntegral nat
sign IntMinus nat = negate $ fromIntegral nat

varIdent :: Lexer (VarIdent, TokenPos)
varIdent = do
  pos <- get
  x <- parseVarIdent
  field @"colNum" += Text.length (simpleVarIdent x)
  pure (x, pos)

typeIdent :: Lexer (TypeIdent, TokenPos)
typeIdent = do
  pos <- get
  x <- parseTypeIdent
  field @"colNum" += Text.length (simpleTypeIdent x)
  pure (x, pos)

cmdIdent :: Lexer (CmdIdent, TokenPos)
cmdIdent = do
  pos <- get
  x <- parseCmdIdent
  field @"colNum" += Text.length (simpleCmdIdent x)
  pure (x, pos)
