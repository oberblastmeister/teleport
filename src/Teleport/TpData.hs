module Teleport.TpData
  ( TpPoint (..),
    TpData (..),
    defaultTpData,
    tpPointPrint,
  )
where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as JSON
import qualified System.Console.ANSI as ANSI

data TpPoint = TpPoint
  { name :: String,
    absFolderPath :: String
  }
  deriving (Show)

instance JSON.FromJSON TpPoint where
  parseJSON (JSON.Object json) =
    TpPoint <$> json .: "name" <*> json .: "absFolderPath"
  parseJSON _ = error "TpPoint must be JSON Object"

instance JSON.ToJSON TpPoint where
  toJSON TpPoint {..} =
    JSON.object
      [ "name" .= name,
        "absFolderPath" .= absFolderPath
      ]

tpPointPrint :: TpPoint -> IO ()
tpPointPrint tpPoint = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
  putStr $ name tpPoint
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  putStr "\t"
  putStr $ absFolderPath tpPoint
  putStr "\n"

newtype TpData = TpData
  { tpPoints :: [TpPoint]
  }
  deriving (Show)

instance JSON.FromJSON TpData where
  parseJSON (JSON.Object v) =
    TpData <$> (v .: "tpPoints")
  parseJSON _ = error "TpData must be JSON Object"

instance JSON.ToJSON TpData where
  toJSON TpData {..} =
    JSON.object ["tpPoints" .= tpPoints]

defaultTpData :: TpData
defaultTpData = TpData []
