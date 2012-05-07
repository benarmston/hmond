{-# LANGUAGE RecordWildCards #-}

module Hmond.Output ( generateXML
                    , ) where


import Data.Maybe (fromMaybe)
import Data.Time.Format
import System.Locale

import Text.XML.Generator

import Hmond.Types

generateXML :: (FormatTime t, XmlOutput x) => t -> ClusterInfo -> [Host] -> x
generateXML now clusterInfo hosts = xrender $ doc defaultDocInfo $
    xelem "GANGLIA_XML" $
        attributes [ ("VERSION", "2.5.7")
                   , ("SOURCE", "hmond")
                   ] <#>
                       clusterXml now clusterInfo hosts


clusterXml :: FormatTime t => t -> ClusterInfo -> [Host] -> Xml Elem
clusterXml now ClusterInfo{..} hosts =
    xelem "CLUSTER" $
        attributes [ ("NAME", clName)
                   , ("OWNER", clOwner)
                   , ("LATLONG", clLatlong)
                   , ("URL", clUrl)
                   , ("LOCALTIME", localtime now)
                   ] <#>
                       (xelems $ map (hostXml now) hosts)


hostXml :: FormatTime t => t -> Host -> Xml Elem
hostXml now Host{..} =
    xelem "HOST" $
        attributes [ ("NAME", hostname)
                   , ("IP", hostIP)
                   , ("LOCATION", "unknown")
                   -- XXX This should probably be when the metric was collected.
                   , ("REPORTED", localtime now)
                   , ("TN", "50")
                   , ("TMAX", "60")
                   , ("DMAX", "600")
                   -- , ("GMOND_STARTED", localtime hmondStartTime)
                   ] <#>
                       (xelems $ map (\(MkM mr _) -> metricXml mr) (hostMetrics))


metricXml :: MetricRecord a -> Xml Elem
metricXml mr@MkMr{..} =
    xelem "METRIC" $
        attributes [ ("NAME", mrName)
                   , ("VAL" , show mrValue)
                   , ("TYPE", mrType mr)
                   , ("UNITS", fromMaybe "" mrUnits)
                   , ("TN", show mrTN)
                   , ("TMAX", show mrTMAX)
                   , ("DMAX", show mrDMAX)
                   , ("SLOPE", show mrSlope)
                   , ("SOURCE", "gmond")
                   ]


localtime :: (FormatTime t) => t -> String
localtime = formatTime defaultTimeLocale "%s"


attributes :: [(String, String)] -> Xml Attr
attributes = xattrs . map (uncurry xattr)
