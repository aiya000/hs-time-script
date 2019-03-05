{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Kinds of namings
module Tim.String where

<<<<<<< HEAD
import Data.Text (Text)
=======
>>>>>>> a9699bd... WIP :+1: Parse variable names strictly
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO
import Tim.Char
import Tim.Megaparsec
import qualified Control.Applicative as P
import qualified Data.String as String
<<<<<<< HEAD
import qualified Data.Text as Text
=======
>>>>>>> a9699bd... WIP :+1: Parse variable names strictly
import qualified Text.Megaparsec.Char as P

-- |
-- Non empty "PascalCase" names
-- (`I "nt"`, `S "tring"`, ...)
data Pascal = Pascal UpperChar [AsciiChar]
  deriving (Show, Eq)

instance Pretty Pascal where
<<<<<<< HEAD
  pretty x = String.fromString . Text.unpack $ unPascal x

unPascal :: Pascal -> Text
unPascal (Pascal x xs) =
  let i = upperToChar x
      dent = Text.pack $ map asciiToChar xs
  in i `Text.cons` dent
=======
  pretty x = String.fromString $ unPascal x

unPascal :: Pascal -> String
unPascal (Pascal x xs) =
  upperToChar x : map asciiToChar xs
>>>>>>> a9699bd... WIP :+1: Parse variable names strictly

parsePascal :: CodeParsing m => m Pascal
parsePascal =
  Pascal <$> upperChar <*> P.many asciiChar


-- | Non empty names
data NonEmpty = NonEmpty Char String
  deriving (Show, Eq)

instance Pretty NonEmpty where
<<<<<<< HEAD
  pretty x = String.fromString . Text.unpack $ unNonEmpty x

unNonEmpty :: NonEmpty -> Text
unNonEmpty (NonEmpty x xs) = Text.pack $ x : xs
=======
  pretty x = String.fromString $ unNonEmpty x

unNonEmpty :: NonEmpty -> String
unNonEmpty (NonEmpty x xs) = x : xs
>>>>>>> a9699bd... WIP :+1: Parse variable names strictly

parseNonEmpty :: CodeParsing m => m NonEmpty
parseNonEmpty =
  NonEmpty <$> P.anyChar <*> P.many P.anyChar

<<<<<<< HEAD
=======
fromString :: String -> Maybe NonEmpty
fromString "" = Nothing
fromString (x : xs) = Just $ NonEmpty x xs

>>>>>>> a9699bd... WIP :+1: Parse variable names strictly

-- | Non empty "camelCase" names
data Camel = Camel AlphaChar [AsciiChar]
  deriving (Show, Eq)

instance Pretty Camel where
<<<<<<< HEAD
  pretty x = String.fromString . Text.unpack $ unCamel x

unCamel :: Camel -> Text
unCamel (Camel x xs) =
  Text.pack $ alphaToChar x : map asciiToChar xs
=======
  pretty x = String.fromString $ unCamel x

unCamel :: Camel -> String
unCamel (Camel x xs) = alphaToChar x : map asciiToChar xs
>>>>>>> a9699bd... WIP :+1: Parse variable names strictly

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
<<<<<<< HEAD
=======

parseLowerString :: CodeParsing m => m LowerString
parseLowerString = LowerString <$> lowerChar <*> P.many lowerChar
>>>>>>> a9699bd... WIP :+1: Parse variable names strictly
