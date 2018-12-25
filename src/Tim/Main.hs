-- | Exposes the standard entry point
module Tim.Main where

import RIO
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger

-- | The standard entry point
app :: RIO Config ()
app = do
  logInfo $ display ("This is a skeleton for tonatona project" :: Text)
  logDebug $ display ("This is a debug message" :: Text)

newtype Config = Config
  { tonaLogger :: TonaLogger.Config
  }

instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasParser Config where
  parser = Config <$> parser
