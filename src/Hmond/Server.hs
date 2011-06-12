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

start :: MVar Env -> IO ()
start envar = withSocketsDo $ do
    listenSock <- listenOn $ PortNumber 8649
    forever $ do
        (handle, _, _) <- accept listenSock
        forkIO $ finally (spewXML handle envar) (hClose handle)
        return ()


spewXML :: Handle -> MVar Env -> IO ()
spewXML handle envar = do
    now <- getCurrentTime
    hosts <- fmap envHosts $ readMVar envar
    BS.hPutStrLn handle $ generateXML now hosts
