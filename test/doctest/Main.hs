module Main where

import RIO
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= runDocTest . workaroundToCompileMain
  where
    -- I don't know but only src/Tim/Main.hs cannot be compiled
    workaroundToCompileMain [] = []
    workaroundToCompileMain (x:xs) =
      if x == "src/Tim/Main.hs"
        then workaroundToCompileMain xs
        else x : workaroundToCompileMain xs

runDocTest :: [String] -> IO ()
runDocTest options =
  doctest $ options <> ghcExtensions

ghcExtensions :: [String]
ghcExtensions =
    [ "-XAutoDeriveTypeable"
    , "-XBangPatterns"
    , "-XBinaryLiterals"
    , "-XConstraintKinds"
    , "-XDataKinds"
    , "-XDefaultSignatures"
    , "-XDeriveDataTypeable"
    , "-XDeriveFoldable"
    , "-XDeriveFunctor"
    , "-XDeriveGeneric"
    , "-XDeriveTraversable"
    , "-XDoAndIfThenElse"
    , "-XDuplicateRecordFields"
    , "-XEmptyDataDecls"
    , "-XExistentialQuantification"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XFunctionalDependencies"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XInstanceSigs"
    , "-XKindSignatures"
    , "-XLambdaCase"
    , "-XMonadFailDesugaring"
    , "-XMultiParamTypeClasses"
    , "-XMultiWayIf"
    , "-XNamedFieldPuns"
    , "-XNoImplicitPrelude"
    , "-XOverloadedStrings"
    , "-XPartialTypeSignatures"
    , "-XPatternGuards"
    , "-XPolyKinds"
    , "-XRankNTypes"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XStandaloneDeriving"
    , "-XTupleSections"
    , "-XTypeApplications"
    , "-XTypeFamilies"
    , "-XTypeSynonymInstances"
    , "-XViewPatterns"
    ]
