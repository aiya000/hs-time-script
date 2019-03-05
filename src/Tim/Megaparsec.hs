module Tim.Megaparsec where

import RIO
import Text.Megaparsec (MonadParsec)

type CodeParsing = MonadParsec Void String
