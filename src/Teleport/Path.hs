module Teleport.Path where

import Control.Monad.IO.Class
import Data.Functor
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LTE
import Path
import Path.IO
import Control.Monad.Catch
import Control.Monad

-- class PathIO p b t where
--   exists :: p b t -> IO Bool

--   absolute :: p b t -> IO (p Abs t)

-- instance PathIO Path Abs File where
--   withPath p f =  parseAbsFile (f p)
-- absolute p = parseAbsFile =<< makeAbsolute (toFilePath p)

-- instance PathIO Path b Dir where
-- absolute p = parseAbsDir =<< makeAbsolute (toFilePath p)

-- home :: IO (Path Abs Dir)
-- home = getHomeDirectory >>= parseAbsDir

-- fileExists :: Path b File -> IO Bool
-- fileExists p = doesFileExist $ toFilePath p

-- dirExists :: Path b Dir -> IO Bool
-- dirExists p = doesDirectoryExist $ toFilePath p

-- realFile :: Path b File -> IO (Path Abs File)
-- realFile p = parseAbsFile =<< makeAbsolute (toFilePath p)

-- realDir :: Path b Dir -> IO (Path Abs Dir)
-- realDir p = parseAbsDir =<< makeAbsolute (toFilePath p)

assertDirExists :: MonadIO m => Path b Dir -> m ()
assertDirExists p = do
  exists <- doesDirExist p
  liftIO $ unless exists (ioError $ userError "Absolute directory does not exist")

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
