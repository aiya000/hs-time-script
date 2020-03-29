module Tim.Megaparsec where

import RIO
import Text.Megaparsec (MonadParsec, Parsec)

type CodeParsing m = (MonadParsec Void String m, MonadFail m)

-- | A concrete of 'CodeParsing'
type CodeParsec = Parsec Void String
