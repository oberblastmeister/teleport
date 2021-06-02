module Main where

import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Filesystem.Path.CurrentOS as Path
import Options.Applicative
import qualified System.Console.ANSI as ANSI
import Teleport
import qualified Turtle
import Prelude hiding (FilePath)

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

main :: IO ()
main = parseOptions >>= run

runAdd :: FilePath -> String -> IO ()
runAdd folderPath addname = do
  dieFolderNotFound folderPath
  tpDataPath <- getTpDataPath
  tpData <- loadTpData tpDataPath
  absFolderPath <- Turtle.realpath folderPath
  let existingTpPoint = find (\tp -> name tp == addname) (tpPoints tpData)
  case existingTpPoint of
    Just tpPoint -> dieTpPointExists tpPoint
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

needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = do
  setErrorColor
  let errorstr = T.pack $ "expected folder, not file: " ++ show path
  Turtle.die errorstr

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = do
  setErrorColor
  let errorStr = T.pack $ "unable to find folder: " ++ show path
  Turtle.die errorStr

dieFolderNotFound :: FilePath -> IO ()
dieFolderNotFound path = do
  folderExists <- Turtle.testdir path
  fileExists <- Turtle.testfile path
  when fileExists (needFolderNotFileError path)
  unless folderExists (folderNotFoundError path)

dieTpPointExists :: TpPoint -> IO ()
dieTpPointExists tpPoint = do
  setErrorColor
  putStrLn $ "teleport point " ++ name tpPoint ++ " already exists:\n"
  tpPointPrint tpPoint
  Turtle.die ""

dieTpPointNotFound :: String -> IO ()
dieTpPointNotFound name = do
  setErrorColor
  let errorname = T.pack (name ++ " tp point not found")
  Turtle.die errorname

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

dieJSONParseError :: FilePath -> String -> IO a
dieJSONParseError jsonFilePath err = do
  let errorstr =
        "parse error in: " ++ show jsonFilePath
          ++ "\nerror:------\n"
          ++ err
  Turtle.die (T.pack errorstr)

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
    Left err -> dieJSONParseError jsonFilePath err
    Right json -> return json

runRemove :: String -> IO ()
runRemove removeName = do
  tpDataPath <- getTpDataPath
  tpData <- loadTpData tpDataPath

  let wantedTpPoint = find (\tp -> name tp == removeName) (tpPoints tpData)
  case wantedTpPoint of
    Nothing -> dieTpPointNotFound removeName
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
    Nothing -> dieTpPointNotFound gotoName
    Just tpPoint -> do
      Turtle.echo $ fromJust $ Turtle.textToLine $ T.pack $ absFolderPath tpPoint
      Turtle.exit $ Turtle.ExitFailure 2
