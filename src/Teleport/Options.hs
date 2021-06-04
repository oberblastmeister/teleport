module Teleport.Options
  ( parseOptions,
    Command (..),
  )
where

import qualified Data.Text as T
import Filesystem.Path.CurrentOS as Path
import Options.Applicative
import Path
import Prelude hiding (FilePath)

data Command
  = CommandList
  | CommandAdd
      { addName :: String,
        folderPath :: SomeBase Dir
      }
  | CommandRemove
      {removeName :: String}
  | CommandGoto
      {gotoName :: String}
  deriving (Show)

parseOptions :: IO Command
parseOptions =
  customExecParser
    (prefs showHelpOnError)
    ( info
        (helper <*> parseCommand)
        ( fullDesc
            <> progDesc tpProgDesc
            <> header tpHeader
        )
    )

parseCommand :: Parser Command
parseCommand =
  subparser $
    command
      "add"
      ( info
          (helper <*> pAdd)
          (fullDesc <> progDesc "add a teleport")
      )
      <> command
        "list"
        ( info
            (helper <*> pList)
            (fullDesc <> progDesc "list all teleport points")
        )
      <> command
        "remove"
        ( info
            (helper <*> pRemove)
            (fullDesc <> progDesc "remove a teleport point")
        )
      <> command
        "goto"
        ( info
            (helper <*> pGoto)
            (fullDesc <> progDesc "go to a create a teleport point")
        )

pList :: Parser Command
pList = pure CommandList

pAdd :: Parser Command
pAdd =
  CommandAdd <$> tpnameParser <*> pDir

pRemove :: Parser Command
pRemove = CommandRemove <$> tpnameParser

pGoto :: Parser Command
pGoto = CommandGoto <$> tpnameParser

tpnameParser :: Parser String
tpnameParser =
  argument
    str
    ( metavar
        "NAME"
        <> help
          "name of the teleport point for usage"
    )

tpProgDesc :: String
tpProgDesc =
  "use teleport to setup teleport points and move to these "
    ++ "when needed"

tpHeader :: String
tpHeader = "Teleport: move around your filesystem"

pDir :: Parser (SomeBase Dir)
pDir =
  argument
    ( str
        >>= readValidSomeDir
    )
    ( value (Rel [reldir|"./"|])
        <> metavar "FOLDERPATH"
        <> help
          ( "path of the teleport folder to teleport to."
              ++ "By default, taken as current working directory"
          )
    )

readFolderPath :: String -> ReadM FilePath
readFolderPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
    then return path
    else readerError ("invalid path: " ++ show path)

readValidSomeDir :: String -> ReadM (SomeBase Dir)
readValidSomeDir s = case parseSomeDir s of
  Left p -> readerError $ "invalid path: " ++ show p
  Right p -> return p
