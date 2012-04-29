{-# LANGUAGE RecordWildCards #-}

module Hmond.Output ( generateXML
                    , ) where


import Data.Maybe (fromMaybe)
import Data.Time.Format
import System.Locale

import Text.XML.Generator

import Hmond.Types

generateXML :: (FormatTime t, XmlOutput x) => t -> ClusterInfo -> [Host] -> x
generateXML now ClusterInfo{..} hosts = xrender $ doc defaultDocInfo $
    xelem "GANGLIA_XML" $ genAttrList [ ("VERSION", "2.5.7")
                                      , ("SOURCE", "hmond")
                                      ] <#>
        (xelem "CLUSTER" $ genAttrList [ ("NAME", clName)
                                       , ("OWNER", clOwner)
                                       , ("LATLONG", clLatlong)
                                       , ("URL", clUrl)
                                       , ("LOCALTIME", localtime now)
                                       ] <#>
            (xelems $ map (hostXml now) hosts ))


hostXml :: FormatTime t => t -> Host -> Xml Elem
hostXml now Host{..} = xelem "HOST" $ genAttrList [ ("NAME", hostname)
                                                  , ("IP", hostIP)
                                                  , ("LOCATION", "unknown")
                                                  , ("REPORTED", localtime now)
                                                  , ("TN", "50")
                                                  , ("TMAX", "60")
                                                  , ("DMAX", "600")
                                                  -- , ("GMOND_STARTED", localtime hmondStartTime)
                                                  ] <#>
                        (xelems $ map (\(MkM mr _) -> metricXml mr) (hostMetrics))


metricXml :: MetricRecord a -> Xml Elem
metricXml mr@MkMr{..} = xelem "METRIC" $ genAttrList [ ("NAME", mrName)
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


genAttrList :: [(String, String)] -> Xml Attr
genAttrList = xattrs . map (uncurry xattr)
