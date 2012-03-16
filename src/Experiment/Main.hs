{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Data.Char
import Control.Monad(forM_)

-- | Types

data ValueGenerator a = ValueGenerator { runGenerator :: (a, ValueGenerator a) }

data Metric a = Metric { metricValue    :: a
                       , metricValueGen :: (ValueGenerator a)
                       }

data Host a = Host { hostMetrics :: [Metric a] }



-- | Smart constructors

--  makeHost = Host numMetrics ::  Host Double
makeHost = Host stringMetrics ::  Host [Char]

numMetrics ::  [Metric Double]
numMetrics = [ makeMetric $ fixedGenerator 1
             , makeMetric $ fixedGenerator 1.5
             , makeMetric $ decrementingGenerator 10
             , makeMetric $ decrementingGenerator 10.5
             ]

stringMetrics ::  [Metric String]
stringMetrics = [ makeMetric $ fixedGenerator "Bob"
                , makeMetric $ caseTogglingGenerator "bob"
                --  This raises a compile time error, due to "10" not
                --  having an instance of Num.
                --  , makeMetric $ decrementingGenerator "10"
                ]

--  Uncommenting this and it doesn't compile.
--  mixedMetrics = numMetrics ++ stringMetrics

makeMetric ::  ValueGenerator a -> Metric a
makeMetric vg = Metric { metricValue = value
                       , metricValueGen = vg
                       }
  where value = fst . runGenerator $ vg



-- | Processing functions

runMetrics ::  Host a -> Host a
runMetrics host = host { hostMetrics = newMetrics}
    where newMetrics = map runMetric $ hostMetrics host


runMetric :: Metric a -> Metric a
runMetric m = newMetric (metricValueGen m)
    where newMetric g = let (val, gen) = runGenerator g in
                            m { metricValue = val
                              , metricValueGen = gen}


-- | Particular value generators
--
fixedGenerator :: a -> ValueGenerator a
fixedGenerator i = ValueGenerator fixedVal
    where fixedVal = (i, fixedGenerator i)


decrementingGenerator :: Num a => a -> ValueGenerator a
decrementingGenerator i = ValueGenerator decrement
    where decrement = (i, decrementingGenerator (i - 1))


caseTogglingGenerator :: String -> ValueGenerator String
caseTogglingGenerator s = ValueGenerator (s, caseTogglingGenerator toggled)
    where toggled = if all isUpper s
                      then map toLower s
                      else map toUpper s


-- | Driver program

main :: IO ()
main = do
    printHostVals makeHost


printHostVals ::  Show a => Host a -> IO b
printHostVals host = do
    forM_ (hostMetrics host) $ \metric ->
        putStr $ show (metricValue metric) ++ " "
    putStrLn ""
    printHostVals $ runMetrics host
