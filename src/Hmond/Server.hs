{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
--  {-# LANGUAGE FlexibleInstances #-}
--  {-# LANGUAGE UndecidableInstances #-}

module Hmond.Server (start) where

import           Control.Concurrent
import           Control.Monad
import           Data.Time.Clock (UTCTime, getCurrentTime)
--  import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Format
import           Network
import           System.IO
import           System.Locale
--  import           System.IO.UTF8           as UTF8

--  import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze.Renderer.Pretty (renderHtml)

import Text.Blaze.GangliaXml
import Text.Blaze.GangliaXml.Attributes

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
    --  now <- getPOSIXTime
    now <- getCurrentTime
    --  UTF8.hPutStrLn handle sampleText
    hPutStrLn handle $ sampleText now
    hClose handle


sampleText :: UTCTime -> String
sampleText now = renderHtml $ do
    ganglia_xml ! source "hmond" ! version "2.5.7" $ do
        cluster ! localtime (toValue now) ! owner "owner" ! latlong "unknown" ! url "unknown" ! name "unspecified" $ do
            host ! ip "192.168.1.1" ! reported (toValue now) ! tn "50" ! tmax "60" ! name "comp02.vm.concurrent-thinking.com" ! dmax "600" $ do
                "It works"


instance ToValue UTCTime where
    toValue = stringValue . formatTime defaultTimeLocale "%s"
