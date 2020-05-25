{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- NOTE:
-- a parser `parseFoo` means a parsing with `try`.
-- a parser `foo` means a parsing without `try`.
module Tim.Lexer.Types
  ( Lexer (..)
  , LexErrorBundle
  , runLexer
  , AtomicLiteral (..)
  , Quote (..)
  , quoteToChar
  , surround
  , pattern String'
  , Token (..)
  , Ident (..)
  , unIdent
  , parseIdent
  , QualifiedIdent (..)
  , unQualifiedIdent
  , parseQualifiedIdent
  , Scope (..)
  , scopeToChar
  , parseScope
  , Register (..)
  , registerToChar
  , parseRegister
  , Option (..)
  , unOption
  , parseOption
  ) where

import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (MonadState)
import Data.Char.Cases hiding (UpperChar(G, S, L, A, V, B, W, T))
import Data.Default (Default (def))
import qualified Data.String as String
import Data.String.Cases
import qualified Data.String.Cases as Name
import Data.String.Here (i)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO hiding (first)
import Text.Megaparsec hiding (Token, SourcePos)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Tim.Megaparsec
import Tim.Processor

type LexErrorBundle = ParseErrorBundle String Void

-- | A context for the lexer
newtype Lexer a = Lexer
  { unLexer :: ParsecT Void String (StateT TokenPos Identity) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadParsec Void String
             , MonadFail
             , MonadState TokenPos
             )

-- | Do tokenize
runLexer :: forall a. Lexer a -> String -> Either LexErrorBundle a
runLexer lexer code = do
  let x = unLexer lexer :: ParsecT Void String (StateT TokenPos Identity) a
  let y = runParserT x "lexer" code :: StateT TokenPos Identity (Either LexErrorBundle a)
  let z = runStateT y def
  fst $ runIdentity z

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

quoteToChar :: Quote -> Char
quoteToChar SingleQ = '\''
quoteToChar DoubleQ = '"'

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
           | Comma -- ^ ,
           | Dot -- ^ .
           | Sharp -- ^ #
           | NewLine -- ^ "\n"
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
  pretty Dot         = String.fromString "."
  pretty Sharp       = String.fromString "#"
  pretty NewLine     = String.fromString "{a line break}"
  pretty (Ident x)   = pretty x
  pretty (Literal x) = pretty x


-- |
-- Any identifiers.
--
-- NOTE:
-- Why identifiers of unqualified variable, types, and commands cannot be determined by the lexer?
-- because these has same representation.
-- unqualified variables and (Vim buildtin) commands has "[a-z][a-zA-Z0-9]*".
-- types and (user defined) commands has "[A-Z][a-zA-Z0-9]*".
data Ident = QualifiedIdent QualifiedIdent
           | UnqualifiedIdent Name.NonEmpty
  deriving (Show, Eq)

instance Pretty Ident where
  pretty (QualifiedIdent x) = pretty x
  pretty (UnqualifiedIdent x) = pretty x

unIdent :: Ident -> String
unIdent (QualifiedIdent x) = unQualifiedIdent x
unIdent (UnqualifiedIdent x) = Name.unNonEmpty x

parseIdent :: CodeParsing m => m Ident
parseIdent =
  QualifiedIdent <$> parseQualifiedIdent <|>
  UnqualifiedIdent <$> parseUnqualifiedIdent

parseUnqualifiedIdent :: forall m. CodeParsing m => m Name.NonEmpty
parseUnqualifiedIdent =
  Name.NonEmpty
    <$> headChar
    <*> P.many tailChar
  where
    headChar :: m Char
    headChar = P.upperChar <|> P.lowerChar <|> P.char '_'

    tailChar :: m Char
    tailChar = headChar <|> P.digitChar <|> P.char '-'

data QualifiedIdent = Scoped Scope String -- ^ g:, l:foo
                    | Register Register -- ^ @+, @u
                    | Option Option -- ^ &nu, &number
  deriving (Show, Eq)

instance Pretty QualifiedIdent where
  pretty (Scoped x name) = String.fromString [i|${scopeToChar x}:${name}|]
  pretty (Register x) = pretty x
  pretty (Option x) = pretty x

unQualifiedIdent :: QualifiedIdent -> String
unQualifiedIdent (Scoped x name) = scopeToChar x : ':' : name
unQualifiedIdent (Register x)    = ['@', registerToChar x]
unQualifiedIdent (Option x)      = unOption x

parseQualifiedIdent :: CodeParsing m => m QualifiedIdent
parseQualifiedIdent =
  uncurry Scoped <$> parseScoped <|>
  Register <$> parseRegister <|>
  Option <$> parseOption

-- | Parses `s:coped_var` or scope (`s:`, `g:`, `b:`, ...)
parseScoped :: CodeParsing m => m (Scope, String)
parseScoped = P.try $ do
  s <- parseScope
  ident <- (unSnake <$> parseSnake) <|> varArg <|> P.string ""
  pure (s, ident)
  where
    varArg = P.string "000" <|> (show <$> P.decimal @_ @_ @_ @Natural)


-- | x:
data Scope = G | S | L | A | V | B | W | T
  deriving (Show, Eq)

scopeToChar :: Scope -> Char
scopeToChar G = 'g'
scopeToChar S = 's'
scopeToChar L = 'l'
scopeToChar A = 'a'
scopeToChar V = 'v'
scopeToChar B = 'b'
scopeToChar W = 'w'
scopeToChar T = 't'

parseScope :: CodeParsing m => m Scope
parseScope =
  P.string "g:" $> G <|>
  P.string "s:" $> S <|>
  P.string "l:" $> L <|>
  P.string "a:" $> A <|>
  P.string "v:" $> V <|>
  P.string "b:" $> B <|>
  P.string "w:" $> W <|>
  P.string "t:" $> T


-- | Please see `:help registers` on Vim
data Register = Unnamed -- ^ ""
              | SmallDelete -- ^ "-
              | ReadOnlyColon -- ^ ":
              | ReadOnlyDot -- ^ ".
              | ReadOnlyPercent -- ^ "%
              | Buffer -- ^ "#
              | Expression -- ^ "=
              | ClipboardStar -- ^ "*
              | ClipboardPlus -- ^ "+
              | BlackHole -- ^ "_
              | Searched -- ^ "/
              | Numeric DigitChar -- ^ "0 ~ "9
              | Alphabetic AlphaChar -- ^ "a ~ "z and "A ~ "Z
  deriving (Show, Eq)

instance Pretty Register where
  pretty x = String.fromString $ '@' : [registerToChar x]

registerToChar :: Register -> Char
registerToChar Unnamed         = '"'
registerToChar SmallDelete     = '-'
registerToChar ReadOnlyColon   = ':'
registerToChar ReadOnlyDot     = '.'
registerToChar ReadOnlyPercent = '%'
registerToChar Buffer          = '#'
registerToChar Expression      = '='
registerToChar ClipboardStar   = '*'
registerToChar ClipboardPlus   = '+'
registerToChar BlackHole       = '_'
registerToChar Searched        = '/'
registerToChar (Numeric x)     = digitToChar x
registerToChar (Alphabetic x)  = alphaToChar x

parseRegister :: CodeParsing m => m Register
parseRegister = P.try $ do
  _ <- P.single '@'
  P.single '"' $> Unnamed <|>
    P.single '-' $> SmallDelete <|>
    P.single ':' $> ReadOnlyColon <|>
    P.single '.' $> ReadOnlyDot <|>
    P.single '%' $> ReadOnlyPercent <|>
    P.single '#' $> Buffer <|>
    P.single '=' $> Expression <|>
    P.single '*' $> ClipboardStar <|>
    P.single '+' $> ClipboardPlus <|>
    P.single '_' $> BlackHole <|>
    P.single '/' $> Searched <|>
    Numeric    <$> digitChar <|>
    Alphabetic <$> alphaChar


-- | &foo &l:bar &g:baz
data Option = UnscopedOption LowerString
            | LocalScopedOption LowerString
            | GlobalScopedOption LowerString
  deriving (Show, Eq)

instance Pretty Option where
  pretty (UnscopedOption x)     = pretty x
  pretty (LocalScopedOption x)  = pretty x
  pretty (GlobalScopedOption x) = pretty x

unOption :: Option -> String
unOption (UnscopedOption x)     = unLowerString x
unOption (LocalScopedOption x)  = unLowerString x
unOption (GlobalScopedOption x) = unLowerString x

parseOption :: CodeParsing m => m Option
parseOption =
  parseLocalScopedOption <|>
  parseGlobalScopedOption <|>
  parseUnscopedOption

parseLocalScopedOption :: CodeParsing m => m Option
parseLocalScopedOption = P.try $ do
  _ <- P.string "&l:"
  LocalScopedOption <$> parseLowerString

parseGlobalScopedOption :: CodeParsing m => m Option
parseGlobalScopedOption = P.try $ do
  _ <- P.string "&g:"
  GlobalScopedOption <$> parseLowerString

parseUnscopedOption :: CodeParsing m => m Option
parseUnscopedOption = P.try $ do
  _ <- P.string "&"
  UnscopedOption <$> parseLowerString
