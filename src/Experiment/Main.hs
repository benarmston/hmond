{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Data.Char
import Control.Monad(forM_)

-- | Types

data MetricValue = MtString String
                 | MtInt Int
                 | MtDouble Double

instance Show MetricValue where
    show (MtString s)    = s
    show (MtInt i)       = show i
    show (MtDouble d)    = show d

data ValueGenerator = ValueGenerator { runGenerator :: (MetricValue, ValueGenerator) }

data Metric = Metric { metricValue    :: MetricValue
                     , metricValueGen :: ValueGenerator
                     }

data Host = Host { hostMetrics :: [Metric] }


--  makeHost = Host numMetrics ::  Host
--  makeHost = Host stringMetrics ::  Host
makeHost = Host mixedMetrics ::  Host

numMetrics ::  [Metric]
numMetrics = [ makeMetric $ fixedGenerator $ MtInt 1
             , makeMetric $ fixedGenerator $ MtDouble 1.5
             , makeMetric $ decrementingGenerator $ MtInt 10
             , makeMetric $ decrementingGenerator $ MtDouble 10.5
             ]

stringMetrics ::  [Metric]
stringMetrics = [ makeMetric $ fixedGenerator $ MtString "Bob"
                , makeMetric $ caseTogglingGenerator $ MtString "bob"
                --  This raises a compile time error, due to "10" not
                --  having an instance of Num.
                --  , makeMetric $ decrementingGenerator $ MtInt "10"
                ]

--  Uncommenting this and it doesn't compile.
mixedMetrics = numMetrics ++ stringMetrics

makeMetric ::  ValueGenerator -> Metric
makeMetric vg = Metric { metricValue = value
                       , metricValueGen = vg
                       }
  where value = fst . runGenerator $ vg



-- | Processing functions

runMetrics ::  Host -> Host
runMetrics host = host { hostMetrics = newMetrics}
    where newMetrics = map runMetric $ hostMetrics host


runMetric :: Metric -> Metric
runMetric m = newMetric (metricValueGen m)
    where newMetric g = let (val, gen) = runGenerator g in
                            m { metricValue = val
                              , metricValueGen = gen}


-- | Particular value generators
--
fixedGenerator :: MetricValue -> ValueGenerator
fixedGenerator i = ValueGenerator fixedVal
    where fixedVal = (i, fixedGenerator i)


-- This doesn't work for MtString. But there is no compile time check.
decrementingGenerator :: MetricValue -> ValueGenerator
decrementingGenerator (MtInt i)    = ValueGenerator decrement
    where decrement = (MtInt i, decrementingGenerator $ MtInt (i - 1))
decrementingGenerator (MtDouble i) = ValueGenerator decrement
    where decrement = (MtDouble i, decrementingGenerator $ MtDouble (i - 1))


-- This only works for MtString. But there is no compile time check.
caseTogglingGenerator :: MetricValue -> ValueGenerator
caseTogglingGenerator (MtString s) = ValueGenerator (MtString s, caseTogglingGenerator toggled)
    where toggled = MtString $ if all isUpper s
                                then map toLower s
                                else map toUpper s


-- | Driver program

main :: IO ()
main = do
    printHostVals makeHost


printHostVals :: Host -> IO ()
printHostVals host = do
    forM_ (hostMetrics host) $ \metric ->
        putStr $ show (metricValue metric) ++ " "
    putStrLn ""
    printHostVals $ runMetrics host
