module Hmond.Metrics ( metrics
                     , runMetrics
                     , metricValue
                     ) where


import Hmond.Types
import Hmond.ValueGenerator
import Hmond.Generators


metrics :: [Metric]
metrics = [ Metric "fixed" "int32" (fixedGenerator 10)
          , Metric "decrementing" "int32" (decrementingGenerator 10)
          ]


runMetrics :: Host -> ([Metric], Host)
runMetrics (Host n ip ms) = (vals, Host n ip ms')
    where (vals, ms') = unzip $ map runMetric ms


runMetric :: Metric -> (Metric, Metric)
runMetric m = case metricValueGen m of
                   (ValueGenerator g) ->
                       (m, m {metricValueGen = ValueGenerator $ execGenerator g})


metricValue :: Metric -> Int
metricValue m = case metricValueGen m of
                     (ValueGenerator g) -> evalGenerator g
