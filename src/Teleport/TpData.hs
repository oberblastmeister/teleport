module Teleport.TpData
  ( TpPoint (..),
    TpData (..),
    defaultTpData,
    tpPointPrint,
  )
where

import Data.Aeson.TH
import GHC.Generics
import qualified System.Console.ANSI as ANSI

data TpPoint = TpPoint
  { name :: String,
    absFolderPath :: String
  }
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''TpPoint)

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

$(deriveJSON defaultOptions ''TpData)

defaultTpData :: TpData
defaultTpData = TpData []
