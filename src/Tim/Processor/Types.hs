{-# LANGUAGE PatternSynonyms #-}

-- | Exposes types for both the lexer and the parser
module Tim.Processor.Types where

import Data.Text (Text)
import RIO
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as P

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
upperToChar A = 'A'
upperToChar B = 'B'
upperToChar C = 'C'
upperToChar D = 'D'
upperToChar E = 'E'
upperToChar F = 'F'
upperToChar G = 'G'
upperToChar H = 'H'
upperToChar I = 'I'
upperToChar J = 'J'
upperToChar K = 'K'
upperToChar L = 'L'
upperToChar M = 'M'
upperToChar N = 'N'
upperToChar O = 'O'
upperToChar P = 'P'
upperToChar Q = 'Q'
upperToChar R = 'R'
upperToChar S = 'S'
upperToChar T = 'T'
upperToChar U = 'U'
upperToChar V = 'V'
upperToChar W = 'W'
upperToChar X = 'X'
upperToChar Y = 'Y'
upperToChar Z = 'Z'

data LowerChar = A_ | B_ | C_ | D_ | E_ | F_ | G_ | H_ | I_ | J_ | K_ | L_ | M_ | N_ | O_ | P_ | Q_ | R_ | S_ | T_ | U_ | V_ | W_ | X_ | Y_ | Z_
  deriving (Show, Eq)

lowerToChar :: LowerChar -> Char
lowerToChar A_ = 'a'
lowerToChar B_ = 'b'
lowerToChar C_ = 'c'
lowerToChar D_ = 'd'
lowerToChar E_ = 'e'
lowerToChar F_ = 'f'
lowerToChar G_ = 'g'
lowerToChar H_ = 'h'
lowerToChar I_ = 'i'
lowerToChar J_ = 'j'
lowerToChar K_ = 'k'
lowerToChar L_ = 'l'
lowerToChar M_ = 'm'
lowerToChar N_ = 'n'
lowerToChar O_ = 'o'
lowerToChar P_ = 'p'
lowerToChar Q_ = 'q'
lowerToChar R_ = 'r'
lowerToChar S_ = 's'
lowerToChar T_ = 't'
lowerToChar U_ = 'u'
lowerToChar V_ = 'v'
lowerToChar W_ = 'w'
lowerToChar X_ = 'x'
lowerToChar Y_ = 'y'
lowerToChar Z_ = 'z'
