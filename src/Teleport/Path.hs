module Teleport.Path where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Functor
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LTE
import Path
import Path.IO
import System.IO.Error

assertDirExists :: MonadIO m => Path b Dir -> m ()
assertDirExists p = do
  exists <- doesDirExist p
  liftIO $
    unless
      exists
      ( ioError $
          mkIOError
            doesNotExistErrorType
            "absolute directory"
            Nothing
            (Just $ toFilePath p)
      )

assertFileExists :: MonadIO m => Path b File -> m ()
assertFileExists p = do
  exists <- doesFileExist p
  liftIO $ unless exists (ioError $ userError "Absolute file does not exist")

makeSomeBaseFileAbs :: MonadIO m => SomeBase File -> m (Path Abs File)
makeSomeBaseFileAbs = \case
  Abs p -> assertFileExists p $> p
  Rel p -> makeAbsolute p

makeSomeBaseDirAbs :: MonadIO m => SomeBase Dir -> m (Path Abs Dir)
makeSomeBaseDirAbs = \case
  Abs p -> assertDirExists p $> p
  Rel p -> makeAbsolute p

getTpDataPath :: IO (Path Abs File)
getTpDataPath = do
  homeDir <- getHomeDir
  return $ homeDir </> [relfile|.tpdata|]

readFileBytes :: Path b File -> IO ByteString
readFileBytes p = LB.readFile $ toFilePath p

readFileUtf8 :: Path b File -> IO Text
readFileUtf8 p = LTE.decodeUtf8 <$> LB.readFile (toFilePath p)

someBaseToFilePath :: SomeBase t -> FilePath
someBaseToFilePath = \case
  Abs p -> toFilePath p
  Rel p -> toFilePath p
