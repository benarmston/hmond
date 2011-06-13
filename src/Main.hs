module Main where

import Control.Concurrent
import Network
import System.Posix

import Hmond.Config (getConfig)
import Hmond.Types
import qualified Hmond.Server as Server
import qualified Hmond.PeriodicMetricMutator as Mutator
import Hmond.Hosts

main :: IO ()
main = withSocketsDo $ do
    installHandler sigPIPE Ignore Nothing
    config <- getConfig "hmond.cfg"
    envar <- newMVar Env { envHosts = hosts }
    forkIO $ Mutator.start envar config
    Server.start envar config
