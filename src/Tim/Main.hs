-- | Exposes the standard entry point
module Tim.Main where

import Data.Bifunctor (first)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (pretty)
import Prelude (putStrLn, print)
import RIO hiding (logInfo, logDebug, first)
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)
import Tim.Lexer (lex)
import Tim.Parser (parse)
import Tim.Parser.Types (AST)
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger

newtype Config = Config
  { tonaLogger :: TonaLogger.Config
  }

instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasParser Config where
  parser = Config <$> parser


-- | The standard entry point
app :: RIO Config ()
app = ask >>= liftIO . runInputT defaultSettings . loop
  where
    loop :: Config -> InputT IO ()
    loop env = do
      maybeInput <- getInputLine "> "
      case maybeInput of
        Nothing -> pure ()
        Just input -> do
          liftIO . runRIO env $ resolve input
          loop env

    resolve :: String -> RIO Config ()
    resolve xs = case words xs of
      [] -> pure ()
      (":ast" : args) -> printAST $ unwords args
      (_ : _) -> liftIO . putStrLn $ "Not implemented yet: " <> xs

    printAST :: String -> RIO Config ()
    printAST code =
      case process $ Text.pack code of
        Left reason -> liftIO $ putStrLn reason
        Right ast -> liftIO $ print ast


type PrettyFailure = String

process :: Text -> Either PrettyFailure AST
process code = do
  x <- first errorBundlePretty $ lex code
  first prettyShow $ parse x
  where
    prettyShow = show . pretty
