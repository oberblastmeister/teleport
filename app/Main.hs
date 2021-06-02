module Main where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as B
import Data.Data (Typeable)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Typeable (cast)
import Filesystem.Path.CurrentOS as Path
import qualified System.Console.ANSI as ANSI
import Teleport
import qualified Turtle
import Prelude hiding (FilePath)

data AppException
  = DirNotFound FilePath
  | FileNotFound FilePath
  | NeedDirNotFile FilePath
  | TpPointNotFound String
  | TpPointExists TpPoint
  | JSONParseError FilePath String
  deriving (Eq, Typeable)

instance Show AppException where
  show (DirNotFound path) = "unable to find directory " ++ show path
  show (FileNotFound path) = "unable to find file " ++ show path
  show (NeedDirNotFile path) = "path " ++ show path ++ " was a directory instead of a file"
  show (TpPointNotFound name) = name ++ " tp point not found"
  show (TpPointExists tp) = "tp point " ++ name tp ++ " already exists!"
  show (JSONParseError path err) =
    "parse error in: " ++ show path
      ++ "\nerror:------\n"
      ++ err

instance Exception AppException where
  toException e = SomeException e
  fromException (SomeException e) = cast e

handleAppException :: AppException -> IO ()
handleAppException e = do
  setErrorColor
  putStrLn $ "error: " ++ show e

main :: IO ()
main = do
  opts <- parseOptions
  catch (run opts) handleAppException

runAdd :: FilePath -> String -> IO ()
runAdd folderPath addname = do
  _ <- throwIO $ DirNotFound folderPath
  tpDataPath <- getTpDataPath
  tpData <- loadTpData tpDataPath
  absFolderPath <- Turtle.realpath folderPath
  let existingTpPoint = find (\tp -> name tp == addname) (tpPoints tpData)
  case existingTpPoint of
    Just tpPoint -> throwIO $ TpPointExists tpPoint
    Nothing -> do
      let newTpPoint =
            TpPoint
              { name = addname,
                absFolderPath = filePathToString absFolderPath
              }
      putStrLn "creating teleport point: \n"
      tpPointPrint newTpPoint
      let newTpData =
            TpData
              { tpPoints = newTpPoint : tpPoints tpData
              }
      saveTpData tpDataPath newTpData

runList :: IO ()
runList = do
  tpDataPath <- getTpDataPath
  tpData <- loadTpData tpDataPath
  let num_points = length $ tpPoints tpData
  putStr "teleport points: "
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  putStr $ "(total " <> show num_points <> ")\n"
  forM_ (tpPoints tpData) tpPointPrint

run :: Command -> IO ()
run = \case
  CommandAdd {..} -> runAdd folderPath addName
  CommandList -> runList
  CommandRemove {..} -> runRemove removeName
  CommandGoto {..} -> runGoto gotoName

setErrorColor :: IO ()
setErrorColor =
  ANSI.setSGR
    [ -- color to set
      ANSI.SetColor
        -- wherther foreground / background should be affected
        ANSI.Foreground
        -- use the "vivid" color versus the muted colord
        ANSI.Vivid
        -- use red
        ANSI.Red
    ]

getTpDataPath :: IO FilePath
getTpDataPath = do
  homeFolder <- Turtle.home
  return $ homeFolder </> ".tpdata"

saveTpData :: FilePath -> TpData -> IO ()
saveTpData jsonFilePath tpData = do
  let dataBytestring = JSON.encode tpData
  Turtle.touch jsonFilePath
  B.writeFile (filePathToString jsonFilePath) dataBytestring

createTpDataFile :: FilePath -> IO ()
createTpDataFile jsonFilePath = saveTpData jsonFilePath defaultTpData

loadTpData :: FilePath -> IO TpData
loadTpData jsonFilePath = do
  exists <- Turtle.testfile jsonFilePath
  if exists
    then decodeTpData jsonFilePath
    else do
      createTpDataFile jsonFilePath
      return defaultTpData

filePathToString :: FilePath -> String
filePathToString = Path.encodeString

decodeTpData :: FilePath -> IO TpData
decodeTpData jsonFilePath = do
  rawInput <- B.readFile (filePathToString jsonFilePath)
  let jsonResult = JSON.eitherDecode' rawInput
  case jsonResult of
    Left err -> throwIO $ JSONParseError jsonFilePath err
    Right json -> return json

runRemove :: String -> IO ()
runRemove removeName = do
  tpDataPath <- getTpDataPath
  tpData <- loadTpData tpDataPath

  let wantedTpPoint = find (\tp -> name tp == removeName) (tpPoints tpData)
  case wantedTpPoint of
    Nothing -> throwIO $ TpPointNotFound removeName
    Just _ -> do
      let newTpPoints = filter (\tp -> name tp /= removeName) (tpPoints tpData)

      let newTpData =
            tpData
              { tpPoints = newTpPoints
              }

      saveTpData tpDataPath newTpData

      ANSI.setSGR
        [ ANSI.SetColor
            ANSI.Foreground
            ANSI.Dull
            ANSI.White
        ]
      putStr "removed teleport point ["
      ANSI.setSGR
        [ ANSI.SetColor
            ANSI.Foreground
            ANSI.Vivid
            ANSI.Blue
        ]
      putStr removeName
      ANSI.setSGR
        [ ANSI.SetColor
            ANSI.Foreground
            ANSI.Dull
            ANSI.White
        ]
      putStr "]"

runGoto :: String -> IO ()
runGoto gotoName = do
  tpDataPath <- getTpDataPath
  tpData <- loadTpData tpDataPath

  let wantedTpPoint = find (\tp -> name tp == gotoName) (tpPoints tpData)
  case wantedTpPoint of
    Nothing -> throwIO $ TpPointNotFound gotoName
    Just tpPoint -> do
      Turtle.echo $ fromJust $ Turtle.textToLine $ T.pack $ absFolderPath tpPoint
      Turtle.exit $ Turtle.ExitFailure 2
