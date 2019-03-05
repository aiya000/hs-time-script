{-# LANGUAGE PatternSynonyms #-}

module Tim.Parser.Types where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Numeric.Natural (Natural)
import RIO
import qualified Tim.Lexer.Types.Idents as Ident

type Code = [Syntax]

-- NOTE: Why don't use Ident.VarIdent at VarIdent,
--       because for usability. (this is exposed to user directly)
-- | The Time script's abstract syntax tree
data AST = Code Code -- ^ Whole of a code
         | Literal Literal
         | VarIdent Text
  deriving (Show, Eq)

-- | Time script's commands (extended Vim's commands)
data Syntax = Let Lhs (Maybe Ident.TypeIdent) Rhs -- ^ let foo: Bar = lit
            | Bar Syntax Syntax -- ^ `cmd1 | cmd2`
  deriving (Show, Eq)

-- | The left hand side
data Lhs = LVar Ident.VarIdent
         | LDestuct (NonEmpty Ident.VarIdent) -- ^ [x, y] of (`let [x, y] = zs`)
  deriving (Show, Eq)

-- | The right hand side
data Rhs = RVar Ident.VarIdent
         | RLit Literal
  deriving (Show, Eq)

data Literal = Nat Natural
             | Int Int
             | Float Double
             | String StringLit
             | List [Literal]
             | Dict (Map StringLit Literal) -- ^ {'foo': 10}
             | Parens Literal -- ^ enclosed literals => `(10)`, `('str')`
  deriving (Show, Eq)

-- | The string literal, like `'foo'` `"bar"`.
data StringLit = SingleQuoted Text
               | DoubleQuoted Text
  deriving (Show, Eq, Ord)
