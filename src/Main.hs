module Main where

import Control.Concurrent
import Network
import System.Posix

import Hmond.Types
import Hmond.Server (start)
import Hmond.Hosts

main :: IO ()
main = withSocketsDo $ do
    installHandler sigPIPE Ignore Nothing
    envar <- newMVar Env { envHosts = hosts }
    start envar
