module Main where

import Control.Concurrent
import Network
import System.Console.CmdArgs
import System.Posix

import Hmond.Config (getConfig)
import Hmond.Options (options, optionsConf)
import Hmond.Types
import qualified Hmond.Server as Server
import qualified Hmond.PeriodicMetricMutator as Mutator
import Hmond.Hosts

main :: IO ()
main = withSocketsDo $ do
    installHandler sigPIPE Ignore Nothing
    config <- cmdArgs_ options >>= getConfig . optionsConf
    envar <- newMVar Env { envHosts = hosts }
    startupMessage config
    forkIO $ Mutator.start envar config
    Server.start envar config


startupMessage :: Config -> IO ()
startupMessage config = do
    putStrLn $ "Running on port " ++ show (cfgPort config)
