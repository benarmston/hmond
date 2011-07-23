{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Hmond.Types ( Host (..)
                   , Metric (..)
                   , nullMetric
                   , MetricSlope (..)
                   , MetricType (..)
                   , ValueGenerator  (..)
                   , Env (..)
                   , Config (..)
                   , ClusterInfo (..)
                   ) where

import Data.Time.Clock
import Network

data Host = Host { hostname    :: String
                 , hostIP      :: String
                 , hostMetrics :: [Metric]
                 } deriving Show


data Metric = Metric { metricName     :: String
                     , metricType     :: MetricType
                     , metricValue    :: Int
                     , metricValueGen :: ValueGenerator
                     , metricUnits    :: Maybe String
                     , metricTN       :: Int
                     , metricTMAX     :: Int
                     , metricDMAX     :: Int
                     , metricSlope    :: MetricSlope
                     } deriving Show


nullMetric :: Metric
nullMetric = Metric { metricUnits = Nothing
                    , metricTN = 60
                    , metricTMAX = 60
                    , metricDMAX = 180
                    , metricSlope = Unspecified
                    }


data MetricType = MtString |
                  MtInt8   | MtUInt8  |
                  MtInt16  | MtUInt16 |
                  MtInt32  | MtUInt32 |
                  MtFloat  | MtDouble |
                  MtTimestamp

instance Show MetricType where
    show MtString    = "string"
    show MtInt8      = "int8"
    show MtUInt8     = "uint8"
    show MtInt16     = "int16"
    show MtUInt16    = "uint16"
    show MtInt32     = "int32"
    show MtUInt32    = "uint32"
    show MtFloat     = "float"
    show MtDouble    = "double"
    show MtTimestamp = "timestamp"


data MetricSlope = Zero | Positive | Negative | Both | Unspecified

instance Show MetricSlope where
    show Zero        = "zero"
    show Positive    = "positive"
    show Negative    = "negative"
    show Both        = "both"
    show Unspecified = "unspecified"


data ValueGenerator = ValueGenerator { runGenerator :: (Int, ValueGenerator) }


instance Show ValueGenerator where
    show (ValueGenerator _) = "Unknown ValueGenerator"


data Env = Env {
    envHosts :: [Host]
    }


data Config = Config { cfgPort :: PortNumber
                     , cfgMetricUpdatePeriod :: NominalDiffTime
                     , cfgCluster :: ClusterInfo
                     } deriving (Show)


data ClusterInfo = ClusterInfo { clName :: String
                               , clOwner :: String
                               , clLatlong :: String
                               , clUrl :: String
                               } deriving (Show)
