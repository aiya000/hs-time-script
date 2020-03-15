{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Kinds of namings
module Tim.String where

import qualified Data.String as String
import Data.Text.Prettyprint.Doc (Pretty(..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import RIO hiding (NonEmpty)
import qualified Text.Megaparsec as P
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

-- |
-- Simular to 'nonEmptyQ',
-- but naming outsides of 'Pascal' will be rejected.
--
-- >>> [pascalQ|Pascal|]
-- Pascal P [AlphaNumAlpha (AlphaLower A_),AlphaNumAlpha (AlphaLower S_),AlphaNumAlpha (AlphaLower C_),AlphaNumAlpha (AlphaLower A_),AlphaNumAlpha (AlphaLower L_)]
pascalQ :: QuasiQuoter
pascalQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "pascalQ required a non empty string, but the empty string is specified."
    expQ (x : xs) = do
      z <- (quoteExp upperCharQ) [x]
      zs <- mapM (quoteExp alphaNumCharQ) $ map (: []) xs
      pure $ ConE (mkName "Pascal") `AppE` z `AppE` ListE zs


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

-- |
-- Makes a non empty string from String on the compile time.
-- Also throws compile error if the empty string is passed.
--
-- >>> [nonEmptyQ|x|]
-- NonEmpty 'x' ""
--
-- >>> [nonEmptyQ|foo|]
-- NonEmpty 'f' "oo"
--
-- >>> [nonEmptyQ|Bar|]
-- NonEmpty 'B' "ar"
nonEmptyQ :: QuasiQuoter
nonEmptyQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "nonEmptyQ required a non empty string, but the empty string is specified."
    expQ (x : xs) =
      pure $ ConE (mkName "NonEmpty")
        `AppE` LitE (CharL x)
        `AppE` ListE (map (LitE . CharL) xs)


-- | Non empty camelCase names "[a-zA-Z][a-zA-Z0-9]*"
data Camel = Camel AlphaChar [AlphaNumChar]
  deriving (Eq)

-- To easy to debug.
--
-- To strictly check, remove this instance and use `deriving (Show)`.
instance Show Camel where
  show (Camel x xs) = '"' : alphaToChar x : map alphaNumToChar xs <> "\""

instance Pretty Camel where
  pretty = String.fromString . unCamel

unCamel :: Camel -> String
unCamel (Camel x xs) = alphaToChar x : map alphaNumToChar xs

parseCamel :: CodeParsing m => m Camel
parseCamel =
  Camel <$> alphaChar <*> P.many alphaNumChar

-- |
-- Simular to 'nonEmptyQ',
-- but naming outsides of 'Camel' will be rejected.
--
-- >>> [camelQ|camel|]
-- "camel"
--
-- >>> [camelQ|Pascal|]
-- "Pascal"
camelQ :: QuasiQuoter
camelQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "camelQ required a non empty string, but the empty string is specified."
    expQ (x : xs) = do
      z <- (quoteExp alphaCharQ) [x]
      zs <- mapM (quoteExp alphaNumCharQ) $ map (: []) xs
      pure $ ConE (mkName "Camel") `AppE` z `AppE` ListE zs


-- | Non empty sneak_case names "[a-zA-Z_][a-zA-Z0-9_]*"
data Sneak = Sneak SneakHeadChar [SneakChar]
  deriving (Show, Eq)

instance Pretty Sneak where
  pretty = String.fromString . unSneakCase

unSneakCase :: Sneak -> String
unSneakCase (Sneak x xs) =
  unSneakHeadChar x : map unSneakChar xs

parseSneakCase :: CodeParsing m => m Sneak
parseSneakCase =
  Sneak <$>
  sneakHeadChar <*>
  P.many sneakChar

-- |
-- Simular to 'nonEmptyQ',
-- but naming outsides of 'Sneak' will be rejected.
--
-- >>> [sneakQ|foo_bar|]
-- Sneak (SneakHeadAlpha (AlphaLower F_)) [SneakAlphaNum (AlphaNumAlpha (AlphaLower O_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower O_)),SneakUnderscore,SneakAlphaNum (AlphaNumAlpha (AlphaLower B_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower A_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower R_))]
--
-- >>> [sneakQ|__constructor|]
-- Sneak SneakHeadUnderscore [SneakUnderscore,SneakAlphaNum (AlphaNumAlpha (AlphaLower C_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower O_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower N_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower S_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower T_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower R_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower U_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower C_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower T_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower O_)),SneakAlphaNum (AlphaNumAlpha (AlphaLower R_))]
--
-- >>> [sneakQ|FOO_MEE_9|]
-- Sneak (SneakHeadAlpha (AlphaUpper F)) [SneakAlphaNum (AlphaNumAlpha (AlphaUpper O)),SneakAlphaNum (AlphaNumAlpha (AlphaUpper O)),SneakUnderscore,SneakAlphaNum (AlphaNumAlpha (AlphaUpper M)),SneakAlphaNum (AlphaNumAlpha (AlphaUpper E)),SneakAlphaNum (AlphaNumAlpha (AlphaUpper E)),SneakUnderscore,SneakAlphaNum (AlphaNumDigit D9)]
sneakQ :: QuasiQuoter
sneakQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "sneakQ required a non empty string, but the empty string is specified."
    expQ (x : xs) = do
      z <- (quoteExp sneakHeadCharQ) [x]
      zs <- mapM (quoteExp sneakCharQ) $ map (: []) xs
      pure $ ConE (mkName "Sneak") `AppE` z `AppE` ListE zs


-- | Non empty "veryflatten" names
data LowerString = LowerString LowerChar [LowerChar]
  deriving (Show, Eq)

instance Pretty LowerString where
  pretty (LowerString x xs) = String.fromString $ map lowerToChar (x : xs)

unLowerString :: LowerString -> String
unLowerString (LowerString x xs) = lowerToChar x : map lowerToChar xs

parseLowerString :: CodeParsing m => m LowerString
parseLowerString = LowerString <$> lowerChar <*> P.many lowerChar
