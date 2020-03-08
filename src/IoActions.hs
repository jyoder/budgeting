module IoActions (make) where

import qualified Actions
import qualified Argument
import qualified Control.Monad.Except as CME
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding.Error as DTEE
import qualified Error
import qualified FileFormat
import qualified Path
import Protolude
import qualified Result

make :: Actions.T IO
make = Actions.T _getArguments _read _print

_getArguments :: IO [Argument.T]
_getArguments = do
  args <- getArgs
  return $ Argument.fromText . DT.pack <$> args

_read :: Path.T -> CME.ExceptT Error.T IO Text
_read = ExceptT . _read'

_read' :: Path.T -> IO (Result.T Text)
_read' path = do
  fileData <- try $ DBL.readFile filePath :: IO (Either IOException DBL.ByteString)
  return $ either (Result.error . show) (Result.success . _decode) fileData
  where
    filePath = Path.toFilePath path

_decode :: DBL.ByteString -> Text
_decode = DTE.decodeUtf8With DTEE.lenientDecode . DBL.toStrict . FileFormat.dropByteOrderMark

_print :: Text -> IO ()
_print = putStrLn
