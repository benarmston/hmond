{-# LANGUAGE ExistentialQuantification #-}


module Hmond.Types ( Host (..)
                   , Metric (..)
                   , ValueGenerator  (..)
                   , ValueGenerator_ (..)
                   , Env (..)
                   ) where


data Host = Host { hostname    :: String
                 , hostIP      :: String
                 , hostMetrics :: [Metric]
                 } deriving Show


data Metric = Metric { metricName     :: String
                     , metricType     :: String
                     , metricValueGen :: ValueGenerator
                     } deriving Show


data ValueGenerator = forall g. (ValueGenerator_ g, Show g) => ValueGenerator g

instance Show ValueGenerator where
    show (ValueGenerator g) = show g


class ValueGenerator_ g where
    runGenerator :: g -> (Int, g)


data Env = Env {
    envHosts :: [Host]
    }
