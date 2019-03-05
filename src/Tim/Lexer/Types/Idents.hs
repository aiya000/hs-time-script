{-# LANGUAGE PatternSynonyms #-}

module Tim.Lexer.Types.Idents where

import Tim.Char
import Tim.String
import qualified Tim.String as String

type TypeIdent = Pascal

-- | foo, g:bar, v:null, v:true, g:, @", &option, &l:option
type VarIdent = String.NonEmpty

-- | Vim defined or user defined commands
type CmdIdent = Camel

-- TODO: A compile time function for this. like `$(camel "Let")`.
pattern LetIdent :: CmdIdent
pattern LetIdent =
  Camel (AlphaLower L_) [ AsciiAlpha (AlphaLower E_)
                        , AsciiAlpha (AlphaLower T_)
                        ]
