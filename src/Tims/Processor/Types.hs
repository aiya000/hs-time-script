{-# LANGUAGE PatternSynonyms #-}

-- | Exposes types for both the lexer and the parser
module Tims.Processor.Types where

import Data.Text (Text)
import RIO
import qualified Data.Text as Text

-- |
-- PascalCase non empty strings
-- (`I "nt"`, `S "tring"`, ...)
data TypeIdent = TypeIdent UpperChar Text
  deriving (Show, Eq)

simpleTypeIdent :: TypeIdent -> Text
simpleTypeIdent (TypeIdent i dent) = upperToChar i `Text.cons` dent

-- |
-- The non empty string.
--
-- e.g. variables (foo, g:bar, v:null, v:true, @")
data VarIdent = VarIdent Char Text
   deriving (Show, Eq)

simpleVarIdent :: VarIdent -> Text
simpleVarIdent (VarIdent i dent) = i `Text.cons` dent

-- | Vim defined or user defined commands
data CmdIdent = CmdIdent AsciiChar [AsciiChar]
  deriving (Show, Eq)

simpleCmdIdent :: CmdIdent -> Text
simpleCmdIdent (CmdIdent x xs) =
  Text.pack $ asciiToChar x : map asciiToChar xs

pattern LetIdent :: CmdIdent
pattern LetIdent = CmdIdent (Upper L) [Lower E_, Lower T_]

data AsciiChar = Upper UpperChar
               | Lower LowerChar
  deriving (Show, Eq)

asciiToChar :: AsciiChar -> Char
asciiToChar (Upper x) = upperToChar x
asciiToChar (Lower x) = lowerToChar x

data UpperChar = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Show, Eq)

upperToChar :: UpperChar -> Char
upperToChar A = 'a'
upperToChar B = 'b'
upperToChar C = 'c'
upperToChar D = 'd'
upperToChar E = 'e'
upperToChar F = 'f'
upperToChar G = 'g'
upperToChar H = 'h'
upperToChar I = 'i'
upperToChar J = 'j'
upperToChar K = 'k'
upperToChar L = 'l'
upperToChar M = 'm'
upperToChar N = 'n'
upperToChar O = 'o'
upperToChar P = 'p'
upperToChar Q = 'q'
upperToChar R = 'r'
upperToChar S = 's'
upperToChar T = 't'
upperToChar U = 'u'
upperToChar V = 'v'
upperToChar W = 'w'
upperToChar X = 'x'
upperToChar Y = 'y'
upperToChar Z = 'z'

data LowerChar = A_ | B_ | C_ | D_ | E_ | F_ | G_ | H_ | I_ | J_ | K_ | L_ | M_ | N_ | O_ | P_ | Q_ | R_ | S_ | T_ | U_ | V_ | W_ | X_ | Y_ | Z_
  deriving (Show, Eq)

lowerToChar :: LowerChar -> Char
lowerToChar A_ = 'A'
lowerToChar B_ = 'B'
lowerToChar C_ = 'C'
lowerToChar D_ = 'D'
lowerToChar E_ = 'E'
lowerToChar F_ = 'F'
lowerToChar G_ = 'G'
lowerToChar H_ = 'H'
lowerToChar I_ = 'I'
lowerToChar J_ = 'J'
lowerToChar K_ = 'K'
lowerToChar L_ = 'L'
lowerToChar M_ = 'M'
lowerToChar N_ = 'N'
lowerToChar O_ = 'O'
lowerToChar P_ = 'P'
lowerToChar Q_ = 'Q'
lowerToChar R_ = 'R'
lowerToChar S_ = 'S'
lowerToChar T_ = 'T'
lowerToChar U_ = 'U'
lowerToChar V_ = 'V'
lowerToChar W_ = 'W'
lowerToChar X_ = 'X'
lowerToChar Y_ = 'Y'
lowerToChar Z_ = 'Z'
