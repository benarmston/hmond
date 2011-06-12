{-# LANGUAGE RecordWildCards #-}

module Hmond.PeriodicMetricMutator (start) where

import Control.Concurrent
import Control.Monad

import Hmond.Types
import Hmond.Hosts

start :: MVar Env -> IO ()
start envar = do
    forever $ do
        updateMetricValues envar
        threadDelay $ 2 * 1000 * 1000


updateMetricValues ::  MVar Env -> IO ()
updateMetricValues envar = modifyMVar_ envar $ \env@Env{..} ->
    return env {envHosts = runHosts envHosts}
