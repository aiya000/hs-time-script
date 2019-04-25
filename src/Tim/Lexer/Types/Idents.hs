{-# LANGUAGE PatternSynonyms #-}

module Tim.Lexer.Types.Idents where

import RIO
import Tim.Char
import Tim.Megaparsec
import Tim.String
import qualified Control.Applicative as P
import qualified Text.Megaparsec.Char as P
import qualified Tim.String as String

type TypeIdent = Pascal


-- TODO: Support bang
-- | Vim defined or user defined commands
type CmdIdent = Camel

-- TODO: A compile time function for this. like `$(camel "Let")`.
pattern LetIdent :: CmdIdent
pattern LetIdent =
  Camel (AlphaLower L_) [ AsciiAlpha (AlphaLower E_)
                        , AsciiAlpha (AlphaLower T_)
                        ]


-- | foo, g:bar, v:null, v:true, g:, @", &option, &l:option
type VarIdent = String.NonEmpty

parseVarIdent :: CodeParsing m => m VarIdent
parseVarIdent =
  String.NonEmpty
    <$> P.noneOf enclosers
    <*> P.many (P.noneOf enclosers)
  where
    enclosers =
      [ '(', ')'
      , '{', '}'
      , '[', ']'
      ]
