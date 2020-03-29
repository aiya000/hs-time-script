module Tim.Megaparsec where

import RIO
import Text.Megaparsec (MonadParsec)

type CodeParsing m = (MonadParsec Void String m, MonadFail m)
