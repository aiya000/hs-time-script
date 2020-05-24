-- | Exposes contexts for both the lexer and the parser
module Tim.Processor where

import Data.Default (Default(..))
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO

data TokenPos = TokenPos
  { lineNum :: Int
  , colNum  :: Int
  } deriving (Show, Eq, Generic)

instance Default TokenPos where
  def = TokenPos 1 1

instance Pretty TokenPos where
  pretty (TokenPos l c) = "(" <> pretty l <> "," <> pretty c <> ")"
