module IoActions (make) where

import qualified Actions
import qualified Argument
import qualified Control.Monad.Except as Except
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Error
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
  return $ Argument.fromText . Data.Text.pack <$> args

_read :: Path.T -> Except.ExceptT Error.T IO Text
_read = ExceptT . _read'

_read' :: Path.T -> IO (Result.T Text)
_read' path = do
  fileData <- try $ ByteString.readFile filePath :: IO (Either IOException ByteString.ByteString)
  return $ either (Result.error . show) (Result.success . _decode) fileData
  where
    filePath = Path.toFilePath path

_decode :: ByteString.ByteString -> Text
_decode =
  Encoding.decodeUtf8With Error.lenientDecode
    . ByteString.toStrict
    . FileFormat.dropByteOrderMark

_print :: Text -> IO ()
_print = putStrLn
