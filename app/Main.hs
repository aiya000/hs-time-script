module Main where

import RIO
import Tim.Main (app)
import Tonatona (run)

main :: IO ()
main = run app
