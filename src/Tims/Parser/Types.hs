module Tims.Parser.Types where

import Data.Text (Text)
import Numeric.Natural (Natural)

-- | The Time script's abstract syntax tree
newtype AST = AST
  { unAST :: [Syntax]
  } deriving (Show, Eq)

-- | Time script's commands (the extended Vim's commands)
data Syntax = Let VarIdent TypeIdent Literal -- ^ let foo: Bar = lit
            | Bar Syntax Syntax -- ^ `cmd1 | cmd2`
  deriving (Show, Eq)

-- | "Int", "String"
newtype TypeIdent = TypeIdent
  { unTypeIdent :: Text
  } deriving (Show, Eq)

-- | "foo", "g:foo", "l:", "@\""
newtype VarIdent = VarIdent
  { unVarIdent :: Text
  } deriving (Show, Eq)

data Literal = Nat Natural
             | Int Int
             | Float Float
             | String Text
             | List [Literal]
             | Dict (Map Text Literal)
  deriving (Show, Eq)
