{-# LANGUAGE PatternSynonyms #-}

module Tims.Lexer.Types where

import Data.Default (Default(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Numeric.Natural (Natural)
import RIO
import Tims.Processor.Types
import qualified Tims.Processor.Types as Proc

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
-- (10, -20, 1.0, 1.0e+5, 'str', "str")
--
-- NOTICE: "10" (Int), "'str'" (String), "\"str\"" (String)
data AtomicLiteral = Nat Natural
                   | Int Int
                   | Float Double
                   | String Text
  deriving (Show, Eq)

-- | "Int", "List", "x"
type Identifier = Text

-- | Time script's keywords, identifiers, or else
data Token = VarIdent Proc.VarIdent
           | TypeIdent Proc.TypeIdent
           | Command CmdIdent
           | Colon
           | Assign -- ^ =
           | ListBegin -- ^ [
           | ListEnd -- ^ ]
           | DictBegin -- ^ {
           | DictEnd -- ^ }
           | ParenBegin -- ^ (
           | ParenEnd -- ^ )
           | Literal AtomicLiteral
           | Comma
           | LineBreak -- ^ "\n", "\r", "\r\n"
  deriving (Show, Eq)

instance Pretty Token where
  pretty _ = undefined
