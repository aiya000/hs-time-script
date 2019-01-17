{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Exposes types for identifiers
module Tim.Lexer.Types.Idents
  ( TypeIdent (..)
  , simpleTypeIdent
  , parseTypeIdent
  , VarIdent (..)
  , simpleVarIdent
  , parseVarIdent
  , CmdIdent (..)
  , parseCmdIdent
  , simpleCmdIdent
  , pattern LetIdent
  , AsciiChar (..)
  , asciiToChar
  , asciiChar
  , UpperChar (..)
  , upperToChar
  , upperChar
  , LowerChar (..)
  , lowerToChar
  , lowerChar
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO
import Tim.Lexer.Types.Idents.Chars
import qualified Control.Applicative as P
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as P

-- |
-- PascalCase non empty strings
-- (`I "nt"`, `S "tring"`, ...)
data TypeIdent = TypeIdent UpperChar [AsciiChar]
  deriving (Show, Eq)

instance Pretty TypeIdent where
  pretty x = String.fromString . Text.unpack $ simpleTypeIdent x

simpleTypeIdent :: TypeIdent -> Text
simpleTypeIdent (TypeIdent x xs) =
  let i = upperToChar x
      dent = Text.pack $ map asciiToChar xs
  in i `Text.cons` dent

parseTypeIdent :: CodeParsing m => m TypeIdent
parseTypeIdent =
  TypeIdent <$> upperChar <*> P.many asciiChar

-- |
-- Variable identifiers (non empty strings)
--
-- e.g. variables (foo, g:bar, v:null, v:true, g:, @")
data VarIdent = VarIdent Char String
  deriving (Show, Eq)

instance Pretty VarIdent where
  pretty x = String.fromString . Text.unpack $ simpleVarIdent x

simpleVarIdent :: VarIdent -> Text
simpleVarIdent (VarIdent x xs) = Text.pack $ x : xs

-- | Like Foo, v:foo, or foo
parseVarIdent :: CodeParsing m => m VarIdent
parseVarIdent =
  VarIdent <$> P.anyChar <*> P.many P.anyChar

-- | Vim defined or user defined commands
data CmdIdent = CmdIdent AlphaChar [AsciiChar]
  deriving (Show, Eq)

instance Pretty CmdIdent where
  pretty x = String.fromString . Text.unpack $ simpleCmdIdent x

simpleCmdIdent :: CmdIdent -> Text
simpleCmdIdent (CmdIdent x xs) =
  Text.pack $ alphaToChar x : map asciiToChar xs

parseCmdIdent :: CodeParsing m => m CmdIdent
parseCmdIdent =
  CmdIdent <$> alphaChar <*> P.many asciiChar

pattern LetIdent :: CmdIdent
pattern LetIdent =
  CmdIdent (AlphaLower L_) [ AsciiAlpha (AlphaLower E_)
                           , AsciiAlpha (AlphaLower T_)
                           ]
