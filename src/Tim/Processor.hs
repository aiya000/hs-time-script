-- | Exposes contexts for both the lexer and the parser
module Tim.Processor where

import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (MonadState, get)
import Data.Default (def)
import RIO
import Tim.Lexer.Types (TokenPos, Failure(..))

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

-- | Makes the context into a failure with the lexer's current position
throwAtLexer :: String -> Processor a
throwAtLexer msg = do
  pos <- get
  throwError $ Failure msg pos

-- |
-- Simular to 'throwAtLexer',
-- but throws only for `Nothing`.
failIfNothing :: Maybe a -> String -> Processor a
failIfNothing Nothing msg = throwAtLexer msg
failIfNothing (Just x) _ = pure x
