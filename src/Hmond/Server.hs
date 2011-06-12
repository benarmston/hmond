{-# LANGUAGE RecordWildCards #-}

module Hmond.Server (start) where

import           Control.Concurrent
import           Control.Monad
import           Data.Time.Clock (getCurrentTime)
import           Network
import           System.IO

import qualified Data.ByteString as BS

import Hmond.Types
import Hmond.Hosts
import Hmond.Output

start :: MVar Env -> IO ()
start envar = withSocketsDo $ do
    hSetBuffering stdout LineBuffering
    listenSock <- listenOn $ PortNumber 8649
    forever $ do
        (handle, host, _port) <- accept listenSock
        hSetBuffering handle LineBuffering
        forkIO $ handleClient handle envar >> updateMetricValues envar
        return ()


handleClient :: Handle -> MVar Env -> IO ()
handleClient handle envar = do
    now <- getCurrentTime
    hs <- fmap envHosts $ readMVar envar
    BS.hPutStrLn handle $ generateXML now hs
    hClose handle


updateMetricValues ::  MVar Env -> IO ()
updateMetricValues envar = modifyMVar_ envar $ \env@Env{..} ->
    return env {envHosts = runHosts envHosts}
