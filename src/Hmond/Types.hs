{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Hmond.Types ( Host (..)
                   , Metric (..)
                   , MetricRecord (..)
                   , makeMetric
                   , mrType
                   , MetricSlope (..)
                   , MetricType (..)
                   , ValueGenerator  (..)
                   , Env (..)
                   , Config (..)
                   , ClusterInfo (..)
                   ) where

import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Data.Time.Clock
import Network

data Host = Host { hostname    :: String
                 , hostIP      :: String
                 , hostMetrics :: [Metric]
                 }



data MetricRecord a = MkMr { mrName     :: String
                           , mrValue    :: MetricType a
                           , mrUnits    :: Maybe String
                           , mrTN       :: Int
                           , mrTMAX     :: Int
                           , mrDMAX     :: Int
                           , mrSlope    :: MetricSlope
                           } deriving Show

nullMetricRecord :: MetricRecord a
nullMetricRecord = MkMr { mrUnits = Nothing
                        , mrTN = 60
                        , mrTMAX = 60
                        , mrDMAX = 180
                        , mrSlope = Unspecified
                        }


data Metric = forall a. MkM (MetricRecord a) (MetricRecord a -> Metric)


makeMetric :: (MetricType a -> ValueGenerator a)
           -> MetricType a
           -> String
           -> Metric
makeMetric mkGen mv name = MkM (metricRecord) (metricGenerator $ mkGen mv)
    where metricRecord = nullMetricRecord { mrName = name }
          metricGenerator :: ValueGenerator a -> MetricRecord a -> Metric
          metricGenerator gen mr = MkM mr' $ metricGenerator gen'
              where (mv', gen') = runGenerator gen
                    mr'     = mr { mrValue = mv' }


data MetricType a where
    MtInt8      :: Int8      -> MetricType Int8
    MtInt16     :: Int16     -> MetricType Int16
    MtInt32     :: Int32     -> MetricType Int32
    MtUInt8     :: Word8     -> MetricType Word8
    MtUInt16    :: Word16    -> MetricType Word16
    MtUInt32    :: Word32    -> MetricType Word32
    MtFloat     :: Float     -> MetricType Float
    MtDouble    :: Double    -> MetricType Double
    MtString    :: String    -> MetricType String
    --  MtTimestamp :: Timestamp -> MetricType Timestamp


instance Show (MetricType a) where
    show (MtInt8 i)   = show i
    show (MtInt16 i)  = show i
    show (MtInt32 i)  = show i
    show (MtUInt8 i)  = show i
    show (MtUInt16 i) = show i
    show (MtUInt32 i) = show i
    show (MtFloat i)  = show i
    show (MtDouble i) = show i
    show (MtString i) = i


mrType :: MetricRecord a -> String
mrType mr = case mrValue mr of
                 MtInt8 _   -> "int8"
                 MtInt16 _  -> "int16"
                 MtInt32 _  -> "int32"
                 MtUInt8 _  -> "word8"
                 MtUInt16 _ -> "word16"
                 MtUInt32 _ -> "word32"
                 MtFloat _  -> "float"
                 MtDouble _ -> "double"
                 MtString _ -> "string"

data MetricSlope = Zero | Positive | Negative | Both | Unspecified

instance Show MetricSlope where
    show Zero        = "zero"
    show Positive    = "positive"
    show Negative    = "negative"
    show Both        = "both"
    show Unspecified = "unspecified"


data ValueGenerator a = ValueGenerator {
    runGenerator :: (MetricType a, ValueGenerator a)
    }


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
