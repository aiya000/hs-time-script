-- | Exposes the standard entry point
module Tim.Main where

import Data.Bifunctor (first)
import Data.String.Here (i)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (pretty)
import qualified Options.Applicative as OptParser
import Prelude (putStrLn, print)
import RIO hiding (logInfo, logDebug, first)
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Tim.CLIOptions as CLIOptions
import Tim.CLIOptions (CLIOptions(..))
import Tim.Lexer (lex)
import Tim.Parser (parse)
import Tim.Parser.Types (AST)

-- | The standard entry point
defaultMain :: IO ()
defaultMain = do
  config <- Config <$> OptParser.execParser CLIOptions.cliOptions
  runRIO config app


data Config = Config
  { cliOptions :: CLIOptions
  }

app :: RIO Config ()
app = ask >>= \case
  Config (CLIOptions _ (Just src') _) -> compile src'
  _ -> repl


compile :: FilePath -> RIO Config ()
compile src' = do
  CLIOptions printAst' _ dist' <- asks cliOptions
  let actualDist = getActualDist printAst' src' dist'
  if printAst'
    then compileToAst src' actualDist
    else compileToVim src' actualDist
  where
    getActualDist :: Bool -> FilePath -> Maybe FilePath -> FilePath
    getActualDist printAst'' src'' dist'' =
      let altDist = if printAst''
                      then src'' <> ".ast"
                      else src'' <> ".vim"
      in maybe altDist id dist''

compileToAst :: FilePath -> FilePath -> RIO Config ()
compileToAst src' dist' = do
  code <- readFileUtf8 src'
  case process code of
    Left e -> liftIO $ putStrLn e
    Right ast -> writeFileUtf8 dist' $ tshow ast

compileToVim :: FilePath -> FilePath -> RIO Config ()
compileToVim src' dist' = liftIO . putStrLn $ trimMargins
  [i|
    compiling to .vim doesn't implemented yet.
    expected source: ${src'}
    expected destination: ${dist'}
  |]


repl :: RIO Config ()
repl = ask >>= liftIO . runInputT defaultSettings . loop
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


trimMargins :: String -> String
trimMargins = trim . unlines . map trim . lines
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
