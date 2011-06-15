{-# LANGUAGE RecordWildCards #-}

module Hmond.Output ( generateXML
                    , ) where


import Data.Maybe (fromMaybe)
import Data.Time.Format
import System.Locale

import Text.XML.Generator

import Hmond.Types

generateXML :: (FormatTime t, XmlOutput x) => t -> [Host] -> x
generateXML now hosts = xrender $ doc defaultDocInfo $
    xelem "GANGLIA_XML" $ genAttrList [ ("VERSION", "2.5.7")
                                      , ("SOURCE", "hmond")
                                      ] <#>
        (xelem "CLUSTER" $ genAttrList [ ("NAME", "unspecified")
                                       , ("OWNER", "owner")
                                       , ("LATLONG", "unknown")
                                       , ("URL", "unknown")
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
                        (xelems $ map metricXml (hostMetrics))


metricXml :: Metric -> Xml Elem
metricXml Metric{..} = xelem "METRIC" $ genAttrList [ ("NAME", metricName)
                                                    , ("VAL" , show metricValue)
                                                    , ("TYPE", show metricType)
                                                    , ("UNITS", fromMaybe "" metricUnits)
                                                    , ("TN", show metricTN)
                                                    , ("TMAX", show metricTMAX)
                                                    , ("DMAX", show metricDMAX)
                                                    , ("SLOPE", show metricSlope)
                                                    , ("SOURCE", "gmond")
                                                    ]


localtime :: (FormatTime t) => t -> String
localtime = formatTime defaultTimeLocale "%s"


genAttrList :: [(String, String)] -> Xml Attr
genAttrList = xattrs . map (uncurry xattr)
