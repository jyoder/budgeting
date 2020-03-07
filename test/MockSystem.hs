module MockSystem (T, make, getArguments, read, MockSystem.print, printed) where

import qualified Argument
import qualified MockConsole
import qualified MockEnvironment
import qualified MockFileSystem
import qualified Path
import Protolude
import qualified Result

data T
  = T
      { mockEnvironment :: MockEnvironment.T,
        mockFileSystem :: MockFileSystem.T,
        mockConsole :: MockConsole.T
      }

make :: [Argument.T] -> [(Path.T, Result.T Text)] -> T
make arguments fileMap = T
  { mockEnvironment = MockEnvironment.make arguments,
    mockFileSystem = MockFileSystem.make fileMap,
    mockConsole = MockConsole.make
  }

getArguments :: T -> [Argument.T]
getArguments T {mockEnvironment} = MockEnvironment.getArguments mockEnvironment

read :: T -> Path.T -> Result.T Text
read T {mockFileSystem} = MockFileSystem.read mockFileSystem

print :: T -> Text -> T
print system text = do
  let T {mockConsole} = system
  system {mockConsole = MockConsole.print mockConsole text}

printed :: T -> [Text]
printed T {mockConsole} = MockConsole.printed mockConsole
