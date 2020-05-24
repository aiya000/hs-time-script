-- | Exposes contexts for both the lexer and the parser
module Tim.Processor where

import Control.Monad.Except (MonadError, throwError)
import Data.Default (Default(..))
import qualified Data.String as IsString
import Data.String.Here (i)
import Data.Text.Prettyprint.Doc (Pretty(..))
import RIO

-- | A context for both the lexer and the parser
newtype Processor a = Processor
  { runProcessor :: Either Failure a -- ^ Extracts the lexed/parsed result
  } deriving ( Functor, Applicative, Monad
             , MonadError Failure
             )


data TokenPos = TokenPos
  { lineNum :: Int
  , colNum  :: Int
  } deriving (Show, Eq, Generic)

instance Default TokenPos where
  def = TokenPos 1 1

instance Pretty TokenPos where
  pretty (TokenPos l c) = "(" <> pretty l <> "," <> pretty c <> ")"


data SourcePos = OnAToken TokenPos
               | EOF -- ^ The end of input
  deriving (Show, Eq)

instance Pretty SourcePos where
  pretty (OnAToken x) = pretty x
  pretty EOF = IsString.fromString "EOF"


-- | For error messages
data Failure = Failure
  { what_  :: String  -- ^ What is wrong / Why it is failed
  , where_ :: SourcePos  -- ^ Where it is failed
  } deriving (Show, Eq)

instance Pretty Failure where
  pretty (Failure message EOF) =
    IsString.fromString [i|Failure! At the EOF: ${message}|]
  pretty (Failure message (OnAToken pos)) =
    IsString.fromString [i|Failure! ${show $ pretty pos}: ${message}|]


-- | Makes the context into a failure with a reason by a token.
throwTokenError :: TokenPos -> String -> Processor a
throwTokenError pos msg = throwError $ Failure msg (OnAToken pos)

-- |
-- Includes `Maybe a` to the context of 'Processor'.
--
-- Makes the negative context with 'TokenPos' if `Nothing` specified.
-- `
-- some foo & includeTokenStuff pos "message"
-- `
includeTokenStuff :: TokenPos -> String -> Maybe a -> Processor a
includeTokenStuff pos msg Nothing = throwTokenError pos msg
includeTokenStuff _ _ (Just x) = pure x
