{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- NOTE:
-- a parser `parseFoo` means a parsing with `try`.
-- a parser `foo` means a parsing without `try`.
module Tim.Lexer.Types
  ( LexError
  , LexErrorItem
  , LexErrorFancy
  , Naked
  , Lexed
  , Lexer (..)
  , runLexer
  , AtomicLiteral (..)
  , Quote (..)
  , quoteToChar
  , surround
  , pattern String'
  , Token (..)
  , Ident (..)
  , pattern Let
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

import Control.Monad.Except (MonadError)
import Control.Monad.State.Class (MonadState)
import Data.Bifunctor (first)
import Data.Char.Cases hiding (UpperChar(G, S, L, A, V, B, W, T))
import Data.List.NonEmpty hiding (toList, map)
import qualified Data.List.NonEmpty as List
import qualified Data.String as String
import Data.String.Cases
import qualified Data.String.Cases as Name
import Data.String.Here (i)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO hiding (first)
import RIO.List
import Text.Megaparsec hiding (Token, SourcePos)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Tim.Megaparsec
import Tim.Processor
import Tim.Util.String

type LexError = ParseError String Void
type LexErrorBundle = ParseErrorBundle String Void
type LexErrorItem = ErrorItem (P.Token String)
type LexErrorFancy = ErrorFancy Void

-- | Before 'runNaked'
type Naked = ParsecT Void String Processor

-- | After 'runNaked'
type Lexed = Either LexErrorBundle

runNaked :: String -> Naked a -> Processor (Lexed a)
runNaked code naked = runParserT naked "lexer" code

-- | Takes the last of a taken error
compatible :: LexErrorBundle -> Failure
compatible ParseErrorBundle{..} =
    Failure (argue bundleErrors) (onlyPos bundlePosState)
  where
    onlyPos :: PosState String -> SourcePos
    onlyPos PosState{..} =
      OnAToken $ TokenPos
        (unPos $ sourceLine pstateSourcePos)
        (unPos $ sourceColumn pstateSourcePos)

-- |
-- When the lexer is failure,
-- shows an error message for user.
argue :: List.NonEmpty LexError -> String
argue = intercalate "\n" . toList . fmap (("- " <>) . errorDetail)
  where
    errorDetail :: LexError -> String
    errorDetail (TrivialError offSet maybeActual expected) = contrast offSet maybeActual expected
    errorDetail (FancyError offSet errors) = something offSet errors

    contrast :: Int -> Maybe LexErrorItem -> Set LexErrorItem -> String
    contrast offSet Nothing expected =
        [i|L${offSet}: Expected ${commaSeparated expected}|]
    contrast offSet (Just actual) expected =
      [i|L${offSet}: Got ${visible actual}, but expected ${commaSeparated expected}.|]

    commaSeparated :: Set LexErrorItem -> String
    commaSeparated (toList -> items) =
      enumerateByComma $ map visible items

    visible :: LexErrorItem -> String
    visible (Tokens (x :| xs)) = enumerateByComma . map (: []) $ x : xs
    visible (Label (l :| abel)) = l : abel
    visible EndOfInput = "EOF"

    -- TODO: What is 'FancyError'?
    something :: Int -> Set LexErrorFancy -> String
    something offSet errors = [i|This message is TODO. What is this? offSet=${show offSet}, errors=${show errors}|]


-- | A context for the lexer
newtype Lexer a = Lexer
  { unLexer :: Naked a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFail
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
--
-- NOTE:
-- Why identifiers of unqualified variable, types, and commands cannot be determined by the lexer?
-- because these has same representation.
-- unqualified variables and (Vim buildtin) commands has "[a-z][a-zA-Z0-9]*".
-- types and (user defined) comands has "[A-Z][a-zA-Z0-9]*".
data Ident = QualifiedIdent QualifiedIdent
           | UnqualifiedIdent Name.NonEmpty
  deriving (Show, Eq)

instance Pretty Ident where
  pretty (QualifiedIdent x) = pretty x
  pretty (UnqualifiedIdent x) = pretty x

-- | The identifier of "let"
pattern Let :: Ident
pattern Let = UnqualifiedIdent (Name.NonEmpty 'l' "et")

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
    tailChar = headChar <|> P.digitChar

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
