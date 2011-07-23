module Hmond.Metrics ( metrics
                     , runMetrics
                     ) where

import Hmond.Types
import Hmond.Generators
import Hmond.ValueGenerator (evalGenerator)


metrics :: [Metric]
metrics = [ metric "fixed" MtInt32 (fixedGenerator 10)
          , metric "decrementing" MtInt32 (decrementingGenerator 10)
          ]

metric ::  String -> MetricType -> ValueGenerator -> Metric
metric name _type vg =
    nullMetric { metricName = name
               , metricType = _type
               , metricValue = value
               , metricValueGen = vg
               }
  where value = evalGenerator vg


runMetrics :: Host -> Host
runMetrics host = host { hostMetrics = newMetrics}
    where newMetrics = map runMetric $ hostMetrics host


runMetric :: Metric -> Metric
runMetric m = newMetric (metricValueGen m)
    where newMetric g = let (val, gen) = runGenerator g in
                            m { metricValue = val
                              , metricValueGen = gen}
