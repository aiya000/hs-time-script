-- | Defines about CLI options.
module Tim.CLIOptions where

import Options.Applicative
import RIO

data CLIOptions = CLIOptions
  { printAst :: Bool -- ^ Does Time script compiler make result as the AST code? (or .vim)
  , src  :: Maybe FilePath
  , dist :: Maybe FilePath
  } deriving (Show, Eq)

cliOptions :: ParserInfo CLIOptions
cliOptions = info flags $
  fullDesc <>
  progDesc "A command for Time script compiler and REPL." <>
  header "tims - Time script compiler&REPL."
  where
    flags :: Parser CLIOptions
    flags =
      CLIOptions <$>
        switch printAst' <*>
        optional (strOption src') <*>
        optional (strOption dist')

    printAst' =
      long "print-ast" <>
      short 'a' <>
      help "Print AST of the specified .tim instead of compile."

    src' =
      long "src" <>
      short 's' <>
      metavar "SOURCE" <>
      help "The input source that is a .tim or a directory."

    dist' =
      long "dist" <>
      short 'o' <>  -- meaning 'output'
      metavar "OUTPUT_DIR" <>
      help "The output result of .vim or AST"
