-- | Exposes the standard entry point
module Tim.Main where

import RIO hiding (logInfo, logDebug)
import Tonatona (HasConfig(..), HasParser(..))
import Tonatona.Logger (logInfo, logDebug)
import qualified Tonatona.Logger as TonaLogger

-- | The standard entry point
app :: RIO Config ()
app = do
  logInfo $ display @Text "This is a skeleton for tonatona project"
  logDebug $ display @Text "This is a debug message"

newtype Config = Config
  { tonaLogger :: TonaLogger.Config
  }

instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasParser Config where
  parser = Config <$> parser
