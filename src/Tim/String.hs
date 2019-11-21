{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Kinds of namings
module Tim.String where

import qualified Data.String as String
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO hiding (NonEmpty)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Tim.Char
import Tim.Megaparsec

-- | Non empty PascalCase names "[A-Z][a-zA-Z0-9]*"
data Pascal = Pascal UpperChar [AlphaNumChar]
  deriving (Show, Eq)

instance Pretty Pascal where
  pretty = String.fromString . unPascal

unPascal :: Pascal -> String
unPascal (Pascal x xs) =
  upperToChar x : map alphaNumToChar xs

parsePascal :: CodeParsing m => m Pascal
parsePascal =
  Pascal <$> upperChar <*> P.many alphaNumChar


-- | Non empty names ".+"
data NonEmpty = NonEmpty Char String
  deriving (Show, Eq)

instance Pretty NonEmpty where
  pretty = String.fromString . unNonEmpty

unNonEmpty :: NonEmpty -> String
unNonEmpty (NonEmpty x xs) = x : xs

parseNonEmpty :: CodeParsing m => m NonEmpty
parseNonEmpty =
  NonEmpty <$> P.anySingle <*> P.many P.anySingle

fromString :: String -> Maybe NonEmpty
fromString "" = Nothing
fromString (x : xs) = Just $ NonEmpty x xs


-- | Non empty camelCase names "[a-zA-Z][a-zA-Z0-9]*"
data Camel = Camel AlphaChar [AlphaNumChar]
  deriving (Show, Eq)

instance Pretty Camel where
  pretty = String.fromString . unCamel

unCamel :: Camel -> String
unCamel (Camel x xs) = alphaToChar x : map alphaNumToChar xs

parseCamel :: CodeParsing m => m Camel
parseCamel =
  Camel <$> alphaChar <*> P.many alphaNumChar


-- | Non empty sneak_case names "[a-zA-Z_][a-zA-Z0-9_]*"
data SneakCase = SneakCase HeadSneakCaseChar [SneakCaseChar]
  deriving (Show, Eq)

instance Pretty SneakCase where
  pretty = String.fromString . unSneakCase

unSneakCase :: SneakCase -> String
unSneakCase (SneakCase x xs) =
  unHeadSneakCaseChar x : map unSneakCaseChar xs

parseSneakCase :: CodeParsing m => m SneakCase
parseSneakCase =
  SneakCase <$>
  parseHeadSneakCaseChar <*>
  P.many parseSneakCaseChar

-- | [a-zA-Z_]
data HeadSneakCaseChar = UnderScore'
                       | AlphaChar AlphaChar
  deriving (Show, Eq)

unHeadSneakCaseChar :: HeadSneakCaseChar -> Char
unHeadSneakCaseChar UnderScore' = '_'
unHeadSneakCaseChar (AlphaChar x) = alphaToChar x

parseHeadSneakCaseChar :: CodeParsing m => m HeadSneakCaseChar
parseHeadSneakCaseChar =
  UnderScore' <$ P.char '_' <|>
  AlphaChar <$> alphaChar

-- | [a-zA-Z0-9_]
data SneakCaseChar = UnderScore -- ^ _
                   | AlphaNumChar AlphaNumChar -- ^ [a-zA-Z0-9]
  deriving (Show, Eq)

unSneakCaseChar :: SneakCaseChar -> Char
unSneakCaseChar UnderScore = '_'
unSneakCaseChar (AlphaNumChar x) = alphaNumToChar x

parseSneakCaseChar :: CodeParsing m => m SneakCaseChar
parseSneakCaseChar =
  UnderScore <$ P.char '_' <|>
  AlphaNumChar <$> alphaNumChar


-- | Non empty "veryflatten" names
data LowerString = LowerString LowerChar [LowerChar]
  deriving (Show, Eq)

instance Pretty LowerString where
  pretty (LowerString x xs) = String.fromString $ map lowerToChar (x : xs)

unLowerString :: LowerString -> String
unLowerString (LowerString x xs) = lowerToChar x : map lowerToChar xs

parseLowerString :: CodeParsing m => m LowerString
parseLowerString = LowerString <$> lowerChar <*> P.many lowerChar
