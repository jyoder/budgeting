module Actions2 (T (..), makeIo) where

import qualified Argument
import qualified Control.Monad.Except as CME
import qualified Data.ByteString.Lazy as B
import qualified Data.Text
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as E
import qualified Error
import qualified Path
import Protolude
import qualified Result

data T
  = T
      { getArguments :: IO [Argument.T],
        read :: Path.T -> CME.ExceptT Error.T IO Text,
        print :: Text -> IO ()
      }

makeIo :: T
makeIo = T getArgumentsIo readIo printIo

getArgumentsIo :: IO [Argument.T]
getArgumentsIo = do
  args <- getArgs
  return $ Argument.fromText . Data.Text.pack <$> args

readIo :: Path.T -> CME.ExceptT Error.T IO Text
readIo = ExceptT . _readIo

_readIo :: Path.T -> IO (Result.T Text)
_readIo path = do
  fileData <- try $ B.readFile filePath :: IO (Either IOException B.ByteString)
  return $ either (Result.error . show) (Result.success . _decode) fileData
  where
    filePath = Path.toFilePath path

_decode :: B.ByteString -> Text
_decode = T.decodeUtf8With E.lenientDecode . B.toStrict

printIo :: Text -> IO ()
printIo = putStrLn
