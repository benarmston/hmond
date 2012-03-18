{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Data.Char
import Control.Monad(forM_)

-- | Types

data NumericMetricValue
data StringMetricValue

data MetricValue a where
    MtInt    :: Int    -> MetricValue NumericMetricValue
    MtDouble :: Double -> MetricValue NumericMetricValue
    MtString :: String -> MetricValue StringMetricValue

instance Show (MetricValue a) where
    show (MtString s)    = s
    show (MtInt i)       = show i
    show (MtDouble d)    = show d

data ValueGenerator a = ValueGenerator { runGenerator :: (MetricValue a, ValueGenerator a) }

data Metric a = Metric { metricValue_    :: MetricValue a
                       , metricValueGen_ :: ValueGenerator a
                       }

data Host a = Host { hostMetrics :: [Metric a] }

makeHost = Host numMetrics ::  Host NumericMetricValue
--  makeHost = Host stringMetrics ::  Host StringMetricValue
--  makeHost = Host mixedMetrics ::  Host ?

numMetrics ::  [Metric NumericMetricValue]
numMetrics = [ makeMetric $ fixedGenerator $ MtInt 1
             , makeMetric $ fixedGenerator $ MtDouble 1.5
             , makeMetric $ decrementingGenerator $ MtInt 10
             , makeMetric $ decrementingGenerator $ MtDouble 10.5
             ]

stringMetrics ::  [Metric StringMetricValue]
stringMetrics = [ makeMetric $ fixedGenerator $ MtString "Bob"
                , makeMetric $ caseTogglingGenerator $ MtString "bob"
                --  These raise a compile time error as desired.
                --  , makeMetric $ decrementingGenerator $ MtInt "10"
                --  , makeMetric $ decrementingGenerator $ MtString "10"
                ]

--  Uncommenting this and it doesn't compile.
--  mixedMetrics = numMetrics ++ stringMetrics

makeMetric ::  ValueGenerator a -> Metric a
makeMetric vg = Metric { metricValue_ = value
                       , metricValueGen_ = vg
                       }
  where value = fst . runGenerator $ vg



-- | Processing functions

runMetrics ::  Host a -> Host a
runMetrics host = host { hostMetrics = newMetrics}
    where newMetrics = map runMetric $ hostMetrics host


runMetric :: Metric a -> Metric a
runMetric m = newMetric (metricValueGen_ m)
  where
    newMetric g = let (val, gen) = runGenerator g in
                      m { metricValue_ = val
                        , metricValueGen_ = gen}


-- | Particular value generators
--
fixedGenerator :: MetricValue a -> ValueGenerator a
fixedGenerator i = ValueGenerator fixedVal
    where fixedVal = (i, fixedGenerator i)


-- This doesn't work for MtString. But there is no compile time check.
decrementingGenerator :: MetricValue NumericMetricValue -> ValueGenerator NumericMetricValue
decrementingGenerator (MtInt i)    = ValueGenerator decrement
    where decrement = (MtInt i, decrementingGenerator $ MtInt (i - 1))
decrementingGenerator (MtDouble i) = ValueGenerator decrement
    where decrement = (MtDouble i, decrementingGenerator $ MtDouble (i - 1))


-- This only works for MtString. But there is no compile time check.
caseTogglingGenerator :: MetricValue StringMetricValue -> ValueGenerator StringMetricValue
caseTogglingGenerator (MtString s) = ValueGenerator (MtString s, caseTogglingGenerator toggled)
    where toggled = MtString $ if all isUpper s
                                then map toLower s
                                else map toUpper s


-- | Driver program

main :: IO ()
main = do
    printHostVals makeHost


printHostVals :: Host a -> IO ()
printHostVals host = do
    forM_ (hostMetrics host) $ \metric ->
        putStr $ show (metricValue_ $ metric) ++ " "
    putStrLn ""
    printHostVals $ runMetrics host
