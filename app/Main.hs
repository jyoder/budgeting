module Main where

import qualified Actions
import qualified Application
import Protolude

main :: IO ()
main = Application.run Actions.makeIo
