module Tim.Lexer (lex) where

import Data.Text (Text)
import Tim.Lexer.Types
import Tim.Processor (Processor)
import qualified Prelude as Prelude

-- | Tokenizes a code
lex :: Text -> Processor [(Token, TokenPos)]
lex = Prelude.undefined
