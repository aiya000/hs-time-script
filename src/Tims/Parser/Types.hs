module Tims.Parser.Types where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Numeric.Natural (Natural)
import RIO
import qualified Tims.Processor.Types as Proc

-- | The Time script's abstract syntax tree
newtype AST = AST
  { unAST :: [Syntax]
  } deriving (Show, Eq)

-- | Time script's commands (extended Vim's commands)
data Syntax = Let Lhs (Maybe Proc.TypeIdent) Rhs -- ^ let foo: Bar = lit
            | Bar Syntax Syntax -- ^ `cmd1 | cmd2`
  deriving (Show, Eq)

-- | The left hand side
data Lhs = LVar Proc.VarIdent
         | LDestuct (NonEmpty Proc.VarIdent) -- ^ [x, y] of (`let [x, y] = zs`)
  deriving (Show, Eq)

-- | The right hand side
data Rhs = RVar Proc.VarIdent
         | RLit Literal
  deriving (Show, Eq)

data Literal = Nat Natural
             | Int Int
             | Float Double
             | String Text
             | List [Literal]
             | Dict (Map Text Literal) -- ^ {'foo': 10}
             | Parens Literal -- ^ enclosed literals => `(10)`, `('str')`
  deriving (Show, Eq)
