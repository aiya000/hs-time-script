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
data Pascal = Pascal UpperChar [AsciiChar]
  deriving (Show, Eq)

instance Pretty Pascal where
  pretty x = String.fromString $ unPascal x

unPascal :: Pascal -> String
unPascal (Pascal x xs) =
  upperToChar x : map asciiToChar xs

parsePascal :: CodeParsing m => m Pascal
parsePascal =
  Pascal <$> upperChar <*> P.many asciiChar


-- | Non empty names
data NonEmpty = NonEmpty Char String
  deriving (Show, Eq)

instance Pretty NonEmpty where
  pretty x = String.fromString $ unNonEmpty x

unNonEmpty :: NonEmpty -> String
unNonEmpty (NonEmpty x xs) = x : xs

parseNonEmpty :: CodeParsing m => m NonEmpty
parseNonEmpty =
  NonEmpty <$> P.anyChar <*> P.many P.anyChar

fromString :: String -> Maybe NonEmpty
fromString "" = Nothing
fromString (x : xs) = Just $ NonEmpty x xs


-- | Non empty "camelCase" names
data Camel = Camel AlphaChar [AsciiChar]
  deriving (Show, Eq)

instance Pretty Camel where
  pretty x = String.fromString $ unCamel x

unCamel :: Camel -> String
unCamel (Camel x xs) = alphaToChar x : map asciiToChar xs

parseCamel :: CodeParsing m => m Camel
parseCamel =
  Camel <$> alphaChar <*> P.many asciiChar


-- | Non empty "sneak_case" names
data SneakCase = SneakCase SneakCaseChar [SneakCaseChar]
  deriving (Show, Eq)

data SneakCaseChar = UnderScore -- ^ _
                   | Ascii AsciiChar -- ^ [A-Za-z]
  deriving (Show, Eq)


-- | Non empty "veryflatten" names
data LowerString = LowerString LowerChar [LowerChar]
  deriving (Show, Eq)

parseLowerString :: CodeParsing m => m LowerString
parseLowerString = LowerString <$> lowerChar <*> P.many lowerChar
