module Hmond.Config
    ( Config (..)
    , getConfig
    ) where

import Data.Word
import Data.ConfigFile
import Data.Time.Clock

import Hmond.Types

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- readstring emptyCP contents
        port <- get c "LISTEN" "port"
        update_period <- get c "METRICS" "update_period"
        return Config { cfgPort = fromIntegral (port::Word16)
                      , cfgMetricUpdatePeriod = (realToFrac (update_period::Double))::NominalDiffTime
                      }
  case config of
    Left cperr -> error $ show cperr
    Right config' -> return config'
