module Hmond.Server (start) where

import           Control.Concurrent
import           Control.Monad
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Network
import           System.IO

import qualified Data.ByteString as BS

import Hmond.Types
import Hmond.Hosts
import Hmond.Output

start :: IO ()
start = withSocketsDo $ do
    hSetBuffering stdout LineBuffering
    listenSock <- listenOn $ PortNumber 8649
    forever $ do
        (handle, host, _port) <- accept listenSock
        hSetBuffering handle LineBuffering
        forkIO $ handleClient handle
        return ()


handleClient :: Handle -> IO ()
handleClient handle = do
    now <- getCurrentTime
    BS.hPutStrLn handle $ generateXML now hosts
    hClose handle
