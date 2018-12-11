module Tims.Parser.Types.Idents where

data UpperChar = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Show, Eq)

data LowerChar = A_ | B_ | C_ | D_ | E_ | F_ | G_ | H_ | I_ | J_ | K_ | L_ | M_ | N_ | O_ | P_ | Q_ | R_ | S_ | T_ | U_ | V_ | W_ | X_ | Y_ | Z_
  deriving (Show, Eq)

-- |
-- PascalCase non empty strings
-- (`I "nt"`, `S "tring"`, ...)
data TypeIdent = TypeIdent UpperChar Text
  deriving (Show, Eq)

-- |
-- camelCase non empty strings
-- (`f "oo"`, `v ":exception"`)
--
-- This means all variables
-- (e.g. foo, g:bar, v:null, v:true, @")
data VarIdent = VarIdent LowerChar Text
   deriving (Show, Eq)
