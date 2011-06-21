{-# LANGUAGE RecordWildCards #-}

module Hmond.Server (start) where

import           Control.Concurrent
import           Control.Exception (finally)
import           Control.Monad
import           Data.Time.Clock (getCurrentTime)
import           Network
import           System.IO

import qualified Data.ByteString as BS

import Hmond.Types
import Hmond.Output

start :: MVar Env -> Config -> IO ()
start envar Config{..} = withSocketsDo $ do
    listenSock <- listenOn $ PortNumber cfgPort
    forever $ do
        (handle, _, _) <- accept listenSock
        forkIO $ finally (spewXML handle envar cfgCluster) (hClose handle)
        return ()


spewXML :: Handle -> MVar Env -> ClusterInfo -> IO ()
spewXML handle envar cluster = do
    now <- getCurrentTime
    hosts <- fmap envHosts $ readMVar envar
    BS.hPutStrLn handle $ generateXML now cluster hosts
