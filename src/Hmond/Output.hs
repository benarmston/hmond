module Hmond.Output ( generateXML
                    , ) where


import Data.Time.Format
import System.Locale

import Text.XML.Generator

import Hmond.Types
import Hmond.Hosts
import Hmond.Metrics

generateXML :: (FormatTime t, XmlOutput x) => t -> [Host] -> x
generateXML now hosts = xrender $ doc defaultDocInfo $
    xelem "GANGLIA_XML" $ genAttrList [ ("source", "hmond")
                                      , ("version", "2.5.7")
                                      ] <#>
        (xelem "CLUSTER" $ genAttrList [ ("owner", "owner")
                                       , ("localtime", localtime now)
                                       , ("latlong", "unknown")
                                       , ("url", "unknown")
                                       , ("name", "unspecified")
                                       ] <#>
            (xelems $ map (hostXml now) hosts ))


hostXml :: FormatTime t => t -> Host -> Xml Elem
hostXml now host = xelem "HOST" $ genAttrList [ ("ip", hostIP host)
                                               , ("name", hostname host)
                                               , ("reported", localtime now)
                                               , ("tn", "50")
                                               , ("tmax", "60")
                                               , ("dmax", "600")
                                               ] <#>
                        (xelems $ map metricXml (hostMetrics host))


metricXml :: Metric -> Xml Elem
metricXml metric = xelem "METRIC" $ genAttrList [ ("name", metricName metric)
                                                 , ("type", metricType metric)
                                                 , ("val" , show . metricValue $ metric)
                                                 ]


localtime :: (FormatTime t) => t -> String
localtime = formatTime defaultTimeLocale "%s"


genAttrList :: [(String, String)] -> Xml Attr
genAttrList = xattrs . map (uncurry xattr)
