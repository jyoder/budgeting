module Main where

import qualified Actions2
import qualified Application2
import Protolude

main :: IO ()
main = Application2.run Actions2.makeIo
