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


data Type = Name Camel -- ^ A Name of a higher kind or lower kind type (e.g. `Int`, `List`)
          | App Type [Type] -- ^ An application of a higher kind type and the argument types (e.g. `List Int`, `Dict Bool`)
          | Parens Type -- ^ `(Type)`, `(List X)`, `(List) X`
          | Arrow Type Type -- ^ `X -> Y`
          | Union Type Type -- ^ `X | Y`
  deriving (Show, Eq)

infixr 3 `Arrow`
infixr 4 `Union`
infixl 5 `App`

-- | The parser's variable identifiers
data Variable = SimpleLocal String -- ^ simple_idents
              | Scoped Lexers.Scope String -- ^ g:, l:foo
              | Register Lexers.Register -- ^ @+, @u
              | Option Lexers.Option -- ^ &nu, &number
  deriving (Show, Eq)
