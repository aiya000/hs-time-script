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
lexer = do
  _ <- padding
  x <- P.many $ P.try symbol
        -- symbol <|>
        -- first Literal <$> literal <|>
        -- first Ident <$> ident
  _ <- padding
  pure x


symbol :: Lexer (Token, TokenPos)
symbol =
  listEnd
  where
    (&>>) :: Lexer (a, TokenPos) -> b -> Lexer (b, TokenPos)
    lexer' &>> result = lexer' <&> first (const result)

    -- assignments
    assign = spaces *> char '=' &>> Assign

    -- typing, calling commands
    colon = spaces *> char ':' &>> Colon

    -- destructive assignments, lists, dicts, function arguments&parameter.
    comma = spaces *> char ',' &>> Comma

    -- dict property accessing
    dot = char '.' &>> Dot

    -- autoload names
    sharp = char '#' &>> Sharp

    -- Rhs expressions, around function arguments&parameters, type arguments&parameters.
    -- Both below are allowed.
    -- - echo (10)          , echo(10)
    -- - call f (10)        , call f(10)
    -- - function f (x)     , function f(x)
    -- - Maybe (Either E A) , Maybe(Either E A)
    parenBegin = spaces *> char '(' &>> ParenBegin
    parenEnd   = spaces *> char ')' &>> ParenEnd

    -- destructive assignments, lists, dict index accessing, function options.
    -- Both below are allowed.
    -- - let [x, y]    , let [ x, y ]
    -- - [x, y, z]     , [ x, y, z ]
    -- - xs[x]         , xs[ x ]
    -- - [[no-abort]]  , [ [no-abort] ]    , [ [ no-abort ] ]
    listBegin = spaces *> char '[' &>> ListBegin
    listEnd   = spaces *> char ']' &>> ListEnd

    -- dicts, lambdas
    dictBegin = spaces *> char '{' &>> DictBegin
    dictEnd   = spaces *> char '}' &>> DictEnd

    -- line splitting
    bar = spaces *> char '|' &>> Bar

    -- arrow types, lambdas
    arrow = spaces *> string "->" &>> Arrow


-- | Int literals
literal :: Lexer (AtomicLiteral, TokenPos)
literal = do
  _ <- spaces
  floatLiteral <|>
    natLiteral <|>
    intLiteral <|>
    stringLiteral

natLiteral :: Lexer (AtomicLiteral, TokenPos)
natLiteral =
  first Nat <$> P.try P.decimal `forwardBy` length . show

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
  first Float <$> P.try P.float `forwardBy` length . show

stringLiteral :: Lexer (AtomicLiteral, TokenPos)
stringLiteral = P.try $ do
    (q, pos) <- quote `forward` 1
    -- +1 is a length of `q`
    (str, _) <- flip forwardBy ((+1) . length) $ P.manyTill P.charLiteral $ P.char (quoteToChar q)
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

ident :: Lexer (Ident, TokenPos)
ident = do
  _ <- spaces
  parseIdent `forwardBy` length . unIdent
