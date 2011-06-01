module Hmond.Output ( generateXML
                    , ) where


import Data.Time.Format
import System.Locale

import Text.XML.Generator

import Hmond.Types
import Hmond.Metrics

generateXML :: (FormatTime t, XmlOutput x) => t -> [Host] -> x
generateXML now hosts = xrender $ doc defaultDocInfo $
    xelem "GANGLIA_XML" $ genAttrList [ ("SOURCE", "hmond")
                                      , ("VERSION", "2.5.7")
                                      ] <#>
        (xelem "CLUSTER" $ genAttrList [ ("OWNER", "owner")
                                       , ("LOCALTIME", localtime now)
                                       , ("LATLONG", "unknown")
                                       , ("URL", "unknown")
                                       , ("NAME", "unspecified")
                                       ] <#>
            (xelems $ map (hostXml now) hosts ))


hostXml :: FormatTime t => t -> Host -> Xml Elem
hostXml now host = xelem "HOST" $ genAttrList [ ("IP", hostIP host)
                                              , ("NAME", hostname host)
                                              , ("REPORTED", localtime now)
                                              , ("TN", "50")
                                              , ("TMAX", "60")
                                              , ("DMAX", "600")
                                              ] <#>
                        (xelems $ map metricXml (hostMetrics host))


metricXml :: Metric -> Xml Elem
metricXml metric = xelem "METRIC" $ genAttrList [ ("NAME", metricName metric)
                                                , ("TYPE", metricType metric)
                                                , ("VAL" , show . metricValue $ metric)
                                                ]


localtime :: (FormatTime t) => t -> String
localtime = formatTime defaultTimeLocale "%s"


genAttrList :: [(String, String)] -> Xml Attr
genAttrList = xattrs . map (uncurry xattr)
