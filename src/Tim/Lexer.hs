{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Tim.Lexer (lex) where

import Control.Lens ((+=), (.=))
import Control.Monad.State.Class (get)
import Data.Generics.Product (field)
import Data.Monoid (First(..))
import Data.String.Here (i)
import Data.Text (Text)
import Numeric.Natural (Natural)
import RIO
import Safe (readMay, headMay, tailMay)
import Tim.Lexer.Types
import Tim.Processor (Processor, runProcessor, failIfNothing, Failure, TokenPos)
import qualified Data.Text as Text
import qualified Prelude
import qualified Text.Megaparsec as P
import qualified Tim.Lexer.Types.Idents as Ident

-- | Tokenizes a code
lex :: Text -> Either Failure [(Token, TokenPos)]
lex = runProcessor . rex . Text.unpack

rex :: String -> Processor [(Token, TokenPos)]
rex "" = pure []
rex ('\r' : '\n' : xs) = down xs
rex ('\n' : xs) = down xs
rex ('\r' : xs) = down xs
rex ('=' : xs) = forward Assign 1 xs
rex (':' : xs) = forward Colon 1 xs
rex (',' : xs) = forward Comma 1 xs
rex ('(' : xs) = forward ParenBegin 1 xs
rex (')' : xs) = forward ParenEnd 1 xs
rex ('[' : xs) = forward ListBegin 1 xs
rex (']' : xs) = forward ListEnd 1 xs
rex ('{' : xs) = forward DictBegin 1 xs
rex ('}' : xs) = forward DictEnd 1 xs

-- Int literals
rex ('+' : xs) = rexInt IntPlus xs
rex ('-' : xs) = rexInt IntMinus xs

-- single quoted strings
rex ('\'' : xs) = do
  let (content, rest) = span (/= '\'') xs
  let str = Literal . String $ Text.pack content
  -- Remove the end of '
  rest' <- tailMay rest `failIfNothing` [i|never finite a single quoted string (${content})|]
  forward str (length content) rest'

rex xs = do
  (got, rest) <- headMay (Prelude.lex xs) `failIfNothing` [i|token not found in ${xs}|]
  term <- readTerm got `failIfNothing` [i|an unknown lexer ${got}|]
  forward term (length got) rest
  where
    -- Reads a literal or an identifier
    readTerm :: String -> Maybe Token
    readTerm x = getFirst $
      First (readLit x) <> First (readIdent x)

    readLit :: String -> Maybe Token
    readLit x = fmap Literal . getFirst . mconcat $
      map First [ Nat <$> readMay x
                , Int <$> readMay x
                , Float <$> readMay x
                , String <$> readMay x
                ]

    readIdent :: String -> Maybe Token
    readIdent x = getFirst . mconcat $
      map First [ Type <$> P.parseMaybe Ident.parseTypeIdent x
                , Command <$> P.parseMaybe Ident.parseCmdIdent x
                , Var <$> P.parseMaybe Ident.parseVarIdent x
                ]

forward :: Token -> Int -> String -> Processor [(Token, TokenPos)]
forward token tokenLen rest = do
  token' <- (token,) <$> get
  field @"colNum" += tokenLen
  (token' :) <$> rex rest

down :: String -> Processor [(Token, TokenPos)]
down rest = do
  token' <- (LineBreak,) <$> get
  field @"colNum" .= 1
  field @"lineNum" += 1
  (token' :) <$> rex rest

data IntSign = IntPlus | IntMinus
  deriving (Show, Eq)

doSign :: IntSign -> Natural -> Int
doSign IntPlus nat = fromIntegral nat
doSign IntMinus nat = negate $ fromIntegral nat

rexInt :: IntSign -> String -> Processor [(Token, TokenPos)]
rexInt sign xs = do
  (got, rest) <- headMay (Prelude.lex xs) `failIfNothing` [i|token not found in ${xs}|]
  nat <- readMay @Natural got `failIfNothing` [i|expected a number, but got a non number (${got})|]
  let intLit = Literal . Int $ doSign sign nat
  forward intLit (length got) rest
