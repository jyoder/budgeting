module Main where

import qualified Application
import qualified IoActions
import Protolude

main :: IO ()
main = Application.run IoActions.make
