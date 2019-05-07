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
import RIO.List (foldl)
import Text.Megaparsec (MonadParsec, ParsecT, runParserT, ParseError(..))
import Text.Megaparsec.Error (ErrorItem(..), ErrorFancy(..), errorPos)
import Tim.Megaparsec
import Tim.Processor (Processor, runProcessor, Failure(..), TokenPos(..), SourcePos(..))
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Prelude as Unsafe
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Tim.String as Name

type LexError = ParseError (P.Token String) Void
type LexErrorItem = ErrorItem (P.Token String)
type LexErrorFancy = ErrorFancy Void

takeReasons :: LexError -> Either (Set LexErrorItem) (Set LexErrorFancy)
takeReasons (TrivialError _ _ reasons) = Left reasons
takeReasons (FancyError _ reasons) = Right reasons

takeProblem :: LexError -> Maybe LexErrorItem
takeProblem (TrivialError _ maybeProblem _) = maybeProblem
takeProblem (FancyError _ _) = Nothing

-- | Before 'runNaked'
type Naked = ParsecT Void String Processor

-- | After 'runNaked'
type Lexed = Either LexError

runNaked :: String -> Naked a -> Processor (Lexed a)
runNaked code naked = runParserT naked "lexer" code

-- | Takes the last of a taken error
compatible :: LexError -> Failure
compatible lexError =
  let (P.SourcePos fileName (P.unPos -> line) (P.unPos -> col) :| _) = errorPos lexError
      problem = fromMaybe (Unsafe.error "{has been to unknown conditinon!}") $ takeProblem lexError
      message = [i|Got ${gotten problem}, but expected ${simplizeAll $ takeReasons lexError}|] :: String
  in Failure [i|${fileName}: ${message}|] . OnAToken $ TokenPos line col
  where
    simplizeAll :: Either (Set LexErrorItem) (Set LexErrorFancy) -> String
    simplizeAll =
      unsentences . Set.toList .
        either (Set.map simplizeItem) (Set.map simplizeFancy)

    gotten :: LexErrorItem -> String
    gotten (Tokens (x :| xs)) = [i|`${show . pretty $ x : xs}`|]
    gotten (Label (x :| xs)) = [i|a ${x : xs}|]
    gotten EndOfInput = "EOF"

    simplizeItem :: LexErrorItem -> String
    simplizeItem (Tokens (x :| xs)) = [i|the one character of [${x : xs}]|]
    simplizeItem (Label (x :| xs)) = [i|a ${x : xs}|]
    simplizeItem EndOfInput = "EOF"

    simplizeFancy :: LexErrorFancy -> String
    simplizeFancy = show -- TODO

    unsentences :: [String] -> String
    unsentences [] = ""
    unsentences (x : xs) = foldl (\result y -> result <> ", " <> y) x xs

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
                   | String Quote Text
  deriving (Show, Eq)

instance Pretty AtomicLiteral where
  pretty (Nat x)    = String.fromString $ show x
  pretty (Float x)  = String.fromString $ show x
  pretty (String' q x) = String.fromString $ surround q x
  pretty (Int x) | x < 0 = String.fromString $ show x
                 | otherwise = String.fromString $ '+' : show x

-- | A surround of strings `'` `"`
data Quote = SingleQ | DoubleQ
  deriving (Show, Eq)

toChar :: Quote -> Char
toChar SingleQ = '\''
toChar DoubleQ = '"'

-- | Surrounds the string by a quote character
surround :: Quote -> String -> String
surround SingleQ x = [i|'${x}'|]
surround DoubleQ x = [i|"${x}"|]

-- | Simular to AtomicLiteral's 'String', but from `String`
pattern String' :: Quote -> String -> AtomicLiteral
pattern String' q s <- String q (Text.unpack -> s)
  where
    String' q s = String q (Text.pack s)

{-# COMPLETE Nat, Float, String', Int #-}

-- | "Int", "List", "x"
type Identifier = Text

-- | Time script's keywords, identifiers, or else
data Token = Ident Ident -- ^ An identifier for a command, a variable, or a type.
           | Colon
           | Assign -- ^ =
           | ListBegin -- ^ [
           | ListEnd -- ^ ]
           | DictBegin -- ^ {
           | DictEnd -- ^ }
           | ParenBegin -- ^ (
           | ParenEnd -- ^ )
           | Arrow -- ^ ->
           | Bar -- ^ |
           | Literal AtomicLiteral
           | Comma
           | LineBreak -- ^ "\n", "\r", "\r\n"
  deriving (Show, Eq)

instance Pretty Token where
  pretty Colon       = String.fromString ":"
  pretty Assign      = String.fromString "="
  pretty ListBegin   = String.fromString "["
  pretty ListEnd     = String.fromString "]"
  pretty DictBegin   = String.fromString "{"
  pretty DictEnd     = String.fromString "}"
  pretty ParenBegin  = String.fromString "("
  pretty ParenEnd    = String.fromString ")"
  pretty Arrow       = String.fromString "->"
  pretty Bar         = String.fromString "|"
  pretty Comma       = String.fromString ","
  pretty LineBreak   = String.fromString "{a line break}"
  pretty (Ident x)   = pretty x
  pretty (Literal x) = pretty x


-- |
-- Any identifiers.
-- (command, variable, or type identifier.)
type Ident = Name.NonEmpty

parseIdent :: CodeParsing m => m Ident
parseIdent =
  Name.NonEmpty
    <$> P.noneOf enclosers
    <*> P.many (P.noneOf delimiters)
  where
    delimiters =
      ':' :
      ' ' : enclosers

    enclosers =
      [ '(', ')'
      , '{', '}'
      , '[', ']'
      ]
