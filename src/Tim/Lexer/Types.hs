{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Tim.Lexer.Types where

import Control.Monad.Except (MonadError)
import Control.Monad.State.Class (MonadState)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String.Here (i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Void (Void)
import Numeric.Natural (Natural)
import RIO hiding (first)
import Text.Megaparsec (MonadParsec, ParsecT, runParserT, ParseError)
import Text.Megaparsec.Pos (SourcePos(..))
import Tim.Processor (Processor, runProcessor, Failure(..), TokenPos(..))
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Tim.Lexer.Types.Idents as Ident

-- | Before 'runNaked'
type Naked = ParsecT Void String Processor

-- | After 'runNaked'
type Lexed = Either (ParseError (P.Token String) Void)

runNaked :: String -> Naked a -> Processor (Lexed a)
runNaked code naked = runParserT naked "lexer" code

-- | The compatibility between megaparsec's error and tim's error
compatible :: ParseError (P.Token String) Void -> Failure
compatible e =
  let (SourcePos _ (P.unPos -> line) (P.unPos -> col) :| _) = P.errorPos e
      pos = TokenPos line col
  in Failure (show e) pos


-- | A context for the lexer
newtype Lexer a = Lexer
  { unLexer :: Naked a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadState TokenPos
             , MonadError Failure
             , MonadParsec Void String
             )

-- | Do tokenize
runLexer :: Lexer a -> String -> Either Failure a
runLexer lexer code = unLexer lexer
                      & runNaked code
                      & runProcessor
                      & include
  where
    include :: Either Failure (Lexed a) -> Either Failure a
    include (Left e) = Left e
    include (Right x) = join . Right $ first compatible x


-- |
-- Atomic literals
-- (10, -20, 1.0, 1.0e+5, 'str', "str")
--
-- NOTICE: "10" (Int), "'str'" (String), "\"str\"" (String)
data AtomicLiteral = Nat Natural
                   | Int Int
                   | Float Double
                   | String Text
  deriving (Show, Eq)

-- | Simular to AtomicLiteral's 'String', but from `String`
pattern String' :: String -> AtomicLiteral
pattern String' s <- String (Text.unpack -> s)
  where
    String' s = String (Text.pack s)

-- | "Int", "List", "x"
type Identifier = Text

-- | Time script's keywords, identifiers, or else
data Token = Var Ident.VarIdent
           | Type Ident.TypeIdent
           | Command Ident.CmdIdent
           | Colon
           | Assign -- ^ =
           | ListBegin -- ^ [
           | ListEnd -- ^ ]
           | DictBegin -- ^ {
           | DictEnd -- ^ }
           | ParenBegin -- ^ (
           | ParenEnd -- ^ )
           | Literal AtomicLiteral
           | Comma
           | LineBreak -- ^ "\n", "\r", "\r\n"
  deriving (Show, Eq)

instance Pretty Token where
  pretty _ = [i|TODO (pretty @Token|]
