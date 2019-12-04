{-# LANGUAGE PatternSynonyms #-}

module Tim.Parser.Types where

import qualified Data.List.NonEmpty as List
import Data.Text (Text)
import Numeric.Natural (Natural)
import RIO
import qualified Tim.Lexer.Types as Lexers
import Tim.String

type Code = [Syntax]

-- | The final result of the parser
data AST = Code Code -- ^ Whole of a code
         | Rhs Rhs -- ^ a term
  deriving (Show, Eq)

-- | Time script's commands (extended Vim's commands)
data Syntax = Let Lhs (Maybe Type) Rhs -- ^ let foo: Bar = lit
            | Bar Syntax Syntax -- ^ `cmd1 | cmd2`
  deriving (Show, Eq)

-- | The left hand side
data Lhs = LVar Variable
         | LDestuct (List.NonEmpty Variable) -- ^ [x, y] of (`let [x, y] = zs`)
  deriving (Show, Eq)

-- | The right hand side
data Rhs = RVar Variable
         | RLit Literal
         | RParens Rhs -- ^ enclosed terms => `(10)`, `('str')`
  deriving (Show, Eq)

data Literal = Nat Natural
             | Int Int
             | Float Double
             | String StringLit
             | List [Literal]
             | Dict (Map StringLit Literal) -- ^ {'foo': 10}
  deriving (Show, Eq)


-- | The string literal, like `'foo'` `"bar"`.
data StringLit = SingleQuoted Text
               | DoubleQuoted Text
  deriving (Show, Eq, Ord)


-- | Please see the parser implementation
data Type = Constr Camel [Type]  -- ^ A constructor. e.g. a name "Foo", an applying "Foo Bar"
          | Parens Type
          | Arrow Type Type
          | Union Type Type
  deriving (Show, Eq)

infixr 3 `Arrow`
infixr 4 `Union`

-- | The parser's variable identifiers
data Variable = SimpleLocal String -- ^ simple_idents
              | Scoped Lexers.Scope String -- ^ g:, l:foo
              | Register Lexers.Register -- ^ @+, @u
              | Option Lexers.Option -- ^ &nu, &number
  deriving (Show, Eq)
