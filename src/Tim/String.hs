{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Kinds of namings
module Tim.String where

import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO
import Tim.Char
import Tim.Megaparsec
import qualified Control.Applicative as P
import qualified Data.String as String
import qualified Text.Megaparsec.Char as P

-- |
-- Non empty "PascalCase" names
-- (`I "nt"`, `S "tring"`, ...)
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


-- | Non empty names
data NonEmpty = NonEmpty Char String
  deriving (Show, Eq)

instance Pretty NonEmpty where
  pretty = String.fromString . unNonEmpty

unNonEmpty :: NonEmpty -> String
unNonEmpty (NonEmpty x xs) = x : xs

parseNonEmpty :: CodeParsing m => m NonEmpty
parseNonEmpty =
  NonEmpty <$> P.anyChar <*> P.many P.anyChar

fromString :: String -> Maybe NonEmpty
fromString "" = Nothing
fromString (x : xs) = Just $ NonEmpty x xs


-- | Non empty "camelCase" names
data Camel = Camel AlphaChar [AlphaNumChar]
  deriving (Show, Eq)

instance Pretty Camel where
  pretty = String.fromString . unCamel

unCamel :: Camel -> String
unCamel (Camel x xs) = alphaToChar x : map alphaNumToChar xs

parseCamel :: CodeParsing m => m Camel
parseCamel =
  Camel <$> alphaChar <*> P.many alphaNumChar


-- | Non empty "sneak_case" names
data SneakCase = SneakCase SneakCaseChar [SneakCaseChar]
  deriving (Show, Eq)

instance Pretty SneakCase where
  pretty = String.fromString . unSneakCase

unSneakCase :: SneakCase -> String
unSneakCase (SneakCase x xs) =
  unSneakCaseChar x : map unSneakCaseChar xs

parseSneakCase :: CodeParsing m => m SneakCase
parseSneakCase = do
  x <- parseSneakCaseChar
  xs <- P.many parseSneakCaseChar
  pure $ SneakCase x xs

data SneakCaseChar = UnderScore -- ^ _
                   | AlphaNumChar AlphaNumChar -- ^ [A-Za-z]
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

parseLowerString :: CodeParsing m => m LowerString
parseLowerString = LowerString <$> lowerChar <*> P.many lowerChar
