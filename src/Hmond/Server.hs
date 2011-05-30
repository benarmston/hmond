module Hmond.Server (start, genAttrList, sampleText) where

import           Control.Concurrent
import           Control.Monad
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format
import           Network
import           System.IO
import           System.Locale

import qualified Data.ByteString as BS

import Text.XML.Generator

import Hmond.Types
import Hmond.Hosts

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
    BS.hPutStrLn handle $ sampleText now hosts
    hClose handle


sampleText :: (FormatTime t, XmlOutput x) => t -> [Host] -> x
sampleText now hosts = xrender $ doc defaultDocInfo $
    xelem "GANGLIA_XML" $ genAttrList [ ("source", "hmond")
                                      , ("version", "2.5.7")
                                      ] <#>
        (xelem "CLUSTER" $ genAttrList [ ("owner", "owner")
                                       , ("localtime", localtime now)
                                       , ("latlong", "unknown")
                                       , ("url", "unknown")
                                       , ("name", "unspecified")
                                       ] <#>
            (xelems $ map (\host -> xelem "HOST" $ genAttrList [ ("ip", hostIP host)
                                                               , ("name", hostname host)
                                                               , ("reported", localtime now)
                                                               , ("tn", "50")
                                                               , ("tmax", "60")
                                                               , ("dmax", "600")
                                                               ] <#>
                xtext "It works") hosts))

  where localtime = formatTime defaultTimeLocale "%s"


genAttrList :: [(String, String)] -> Xml Attr
genAttrList = xattrs . map (uncurry xattr)
