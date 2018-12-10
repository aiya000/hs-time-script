-- | Exposes Time script's common types
module Tims.Types where

import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (MonadState)
import Data.Default (def)
import RIO
import Tims.Lexer.Types (TokenPos, Failure)

-- | A context for both the lexer and the parser
newtype Processor a = Processor
  { unProcessor :: ExceptT Failure (State TokenPos) a
  } deriving ( Functor, Applicative, Monad
             , MonadState TokenPos
             , MonadError Failure
             )

-- | Extracts the lexed/parsed result
runProcessor :: Processor a -> Either Failure a
runProcessor = unProcessor
                 >>> (runExceptT :: ExceptT Failure (State TokenPos) a -> State TokenPos (Either Failure a))
                 >>> (flip runState def :: State TokenPos (Either Failure a) -> (Either Failure a, TokenPos))
                 >>> fst
