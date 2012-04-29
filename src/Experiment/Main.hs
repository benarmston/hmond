{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

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

data ValueGenerator a = ValueGenerator { runGenerator :: (MetricValue a, ValueGenerator a) }

data MetricRecord a = MkMr { metricValue_ :: MetricValue a }

data Metric = forall a. MkM (MetricRecord a) (MetricRecord a -> Metric)

data Host = Host { hostMetrics :: [Metric] }


makeHost :: Host
makeHost = Host mixedMetrics

numMetrics ::  [Metric]
numMetrics = [ makeMetric fixedGenerator $ MtInt 1
             , makeMetric fixedGenerator $ MtDouble 1.5
             , makeMetric decrementingGenerator $ MtInt 10
             , makeMetric decrementingGenerator $ MtDouble 10.5
             ]

stringMetrics ::  [Metric]
stringMetrics = [ makeMetric fixedGenerator $ MtString "Bob"
                , makeMetric caseTogglingGenerator $ MtString "bob"
                ]

mixedMetrics :: [Metric]
mixedMetrics = numMetrics ++ stringMetrics

makeMetric :: (MetricValue a -> ValueGenerator a) -> MetricValue a -> Metric
--  XXX not sure about this type signature or mkGen variable name
--  XXX Also change the name of foo.
makeMetric mkGen mv = MkM (MkMr mv) (foo $ mkGen mv)
    where foo :: ValueGenerator a -> MetricRecord a -> Metric
          foo gen mr = MkM mr' $ foo gen'
              where (mv', gen') = runGenerator gen
                    mr'     = mr { metricValue_ = mv' }



-- | Processing functions

runMetrics ::  Host -> Host
runMetrics host = host { hostMetrics = newMetrics}
    where newMetrics = map runMetric $ hostMetrics host
          runMetric (MkM m fn) = fn m


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
    forM_ (hostMetrics host) $ \(MkM mr _) ->
        putStr $ show (metricValue_ $ mr) ++ " "
    putStrLn ""
    printHostVals $ runMetrics host
