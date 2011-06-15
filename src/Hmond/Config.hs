module Hmond.Config
    ( Config (..)
    , getConfig
    ) where

import Control.Monad.Error
import Data.Word
import Data.ConfigFile
import Data.Time.Clock

import Hmond.Types

getConfig :: FilePath -> IO Config
getConfig path = do
  config <- runErrorT $ do
        c <- join $ liftIO $ readfile emptyCP path
        port <- get c "LISTEN" "port"
        update_period <- get c "METRICS" "update_period"
        return Config { cfgPort = fromIntegral (port::Word16)
                      , cfgMetricUpdatePeriod = (realToFrac (update_period::Double))::NominalDiffTime
                      }
  case config of
    Left cperr -> error $ show cperr
    Right config' -> return config'
