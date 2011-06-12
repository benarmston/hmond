{-# LANGUAGE RecordWildCards #-}

module Hmond.PeriodicMetricMutator (start) where

import Control.Concurrent
import Control.Concurrent.Extras

import Hmond.Types
import Hmond.Hosts

start :: MVar Env -> IO ()
start envar = withPeriodicity 2 (updateMetricValues envar)


updateMetricValues ::  MVar Env -> IO ()
updateMetricValues envar = modifyMVar_ envar $ \env@Env{..} ->
    return env {envHosts = runHosts envHosts}
