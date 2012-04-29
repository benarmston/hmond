{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Data.Char
import Control.Monad(forM_)

-- | Types

data MetricValue a where
    MtInt    :: Int    -> MetricValue Int
    MtDouble :: Double -> MetricValue Double
    MtString :: String -> MetricValue String

instance Show (MetricValue a) where
    show (MtString s)    = s
    show (MtInt i)       = show i
    show (MtDouble d)    = show d

type AnyMetric = forall a. Metric a


data ValueGenerator a = ValueGenerator { runGenerator :: (MetricValue a, ValueGenerator a) }

data Metric a = Metric { metricValue_    :: MetricValue a
                       , metricValueGen_ :: ValueGenerator a
                       }

data Host = Host { hostMetrics :: [AnyMetric] }


makeHost = Host numMetrics
--  makeHost = Host stringMetrics
--  makeHost = Host mixedMetrics

numMetrics ::  [AnyMetric]
numMetrics = [ makeMetric $ fixedGenerator $ MtInt 1
             , makeMetric $ fixedGenerator $ MtDouble 1.5
             , makeMetric $ decrementingGenerator $ MtInt 10
             , makeMetric $ decrementingGenerator $ MtDouble 10.5
             ]

stringMetrics ::  [AnyMetric]
stringMetrics = [ makeMetric $ fixedGenerator $ MtString "Bob"
                , makeMetric $ caseTogglingGenerator $ MtString "bob"
                --  This raises a compile time error, due to "10" not
                --  having an instance of Num.
                --  , makeMetric $ decrementingGenerator $ MtInt "10"
                --  , makeMetric $ decrementingGenerator $ MtString "10"
                ]

--  Uncommenting this and it doesn't compile.
--  mixedMetrics :: [MetricValue t]
--  mixedMetrics = numMetrics ++ stringMetrics

makeMetric ::  ValueGenerator a -> AnyMetric
makeMetric vg = Metric { metricValue_ = value
                       , metricValueGen_ = vg
                       }
  where value = fst . runGenerator $ vg



-- | Processing functions

runMetrics ::  Host -> Host
runMetrics host = host { hostMetrics = newMetrics}
    where newMetrics = map runMetric $ hostMetrics host


runMetric :: AnyMetric -> AnyMetric
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


decrementingGenerator :: Num a => MetricValue a -> ValueGenerator a
decrementingGenerator (MtInt i)    = ValueGenerator decrement
    where decrement = (MtInt i, decrementingGenerator $ MtInt (i - 1))
decrementingGenerator (MtDouble i) = ValueGenerator decrement
    where decrement = (MtDouble i, decrementingGenerator $ MtDouble (i - 1))


caseTogglingGenerator :: MetricValue String -> ValueGenerator String
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
        putStr $ show (metricValue_ $ metric) ++ " "
    putStrLn ""
    printHostVals $ runMetrics host
