module Tims.Lexer.Types where

import Data.Default (Default(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO

data TokenPos = TokenPos
  { lineNum :: Int
  , colNum  :: Int
  } deriving (Show, Eq)

instance Default TokenPos where
  def = TokenPos 1 1

instance Pretty TokenPos where
  pretty (TokenPos l c) = "(" <> pretty l <> "," <> pretty c <> ")"

-- | For error messages
data Failure = Failure
  { what_  :: String   -- ^ What is wrong / Why it is failed
  , where_ :: TokenPos -- ^ Where it is failed
  } deriving (Show, Eq)

-- |
-- Atomic literals
-- (10, -20, 1.0, 1.0e+5, 'str', "str", )
--
-- NOTICE: "10" (Int), "'str'" (String), "\"str\"" (String)
type SomeLiteral = Text

-- | "Int", "List", "x"
type Identifier = Text

-- | Time script's keywords, identifiers, or else
data Token = Ident Text -- ^ var names, type names, builtin commands, or keywords
           | Colon
           | Assign -- ^ =
           | Literal SomeLiteral
           | LineBreak -- ^ "\n", "\r", "\r\n"
  deriving (Show, Eq)

instance Pretty Token where
  pretty _ = undefined
