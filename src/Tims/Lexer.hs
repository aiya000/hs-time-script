module Tims.Lexer (lex) where

import Data.Text (Text)
import Tims.Lexer.Types
import Tims.Types
import qualified Prelude as Prelude

-- | Tokenizes a code
lex :: Text -> Processor [(Token, TokenPos)]
lex = Prelude.undefined
