module Actions
  ( Actions (..),
    GetArguments,
    ReadFileData,
    PutString,
    Arguments,
    actionsIo,
  )
where

import Data.ByteString.Lazy as B
import Protolude
import qualified Protolude (getArgs)
import Prelude (String)

data Actions a
  = Actions
      { getArguments :: GetArguments a,
        readFileData :: ReadFileData a,
        putString :: PutString a
      }

type GetArguments m = m Arguments

type ReadFileData m = FilePath -> m (Either String B.ByteString)

type PutString m = String -> m ()

type Arguments = [String]

actionsIo :: Actions IO
actionsIo = Actions getArgumentsIo readFileDataIo putStringIo

getArgumentsIo :: GetArguments IO
getArgumentsIo = Protolude.getArgs

readFileDataIo :: ReadFileData IO
readFileDataIo filePath = do
  fileData <- try $ B.readFile filePath :: IO (Either IOException B.ByteString)
  return $ either (Left . show) Right fileData

putStringIo :: PutString IO
putStringIo = Protolude.putStrLn
