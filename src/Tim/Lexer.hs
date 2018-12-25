module Tim.Lexer (lex) where

import Data.Text (Text)
import RIO
import Tim.Lexer.Types
import qualified Prelude

-- | Tokenizes a code
lex :: Text -> Either Failure [(Token, TokenPos)]
lex = Prelude.undefined
