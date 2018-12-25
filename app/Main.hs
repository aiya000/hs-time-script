module Main where

import RIO
import Tonatona (run)
import Tim.Main (app)

main :: IO ()
main = run app
