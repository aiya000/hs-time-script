{-# LANGUAGE QuasiQuotes #-}

-- | Exposes contexts for both the lexer and the parser
module Tim.Processor where

import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (MonadState, get)
import Data.Default (Default(..))
import Data.String.Here (i)
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO
import qualified Data.String as IsString

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
                 >>> runExceptT'
                 >>> runState'
                 >>> fst
  where
    runExceptT' :: ExceptT Failure (State TokenPos) a -> State TokenPos (Either Failure a)
    runExceptT' = runExceptT

    runState' :: State TokenPos (Either Failure a) -> (Either Failure a, TokenPos)
    runState' = flip runState def

data TokenPos = TokenPos
  { lineNum :: Int
  , colNum  :: Int
  } deriving (Show, Eq, Generic)

instance Default TokenPos where
  def = TokenPos 1 1

instance Pretty TokenPos where
  pretty (TokenPos l c) = "(" <> pretty l <> "," <> pretty c <> ")"

-- | For error messages
data Failure = Failure
  { what_  :: String   -- ^ What is wrong / Why it is failed
  , where_ :: TokenPos -- ^ Where it is failed
  } deriving (Show, Eq)

instance Pretty Failure where
  pretty Failure{..} = IsString.fromString [i|failure! ${show $ pretty where_}: ${what_}|]

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
