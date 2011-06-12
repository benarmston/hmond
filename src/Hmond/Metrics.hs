module Hmond.Metrics ( metrics
                     , runMetrics
                     ) where


import Hmond.Types
import Hmond.Generators


metrics :: [Metric]
metrics = [ Metric "fixed" "int32" 10 (fixedGenerator 10)
          , Metric "decrementing" "int32" 10 (decrementingGenerator 10)
          ]


runMetrics :: Host -> Host
runMetrics host = host { hostMetrics = newMetrics}
    where newMetrics = map runMetric $ hostMetrics host


runMetric :: Metric -> Metric
runMetric m = case metricValueGen m of
                   (ValueGenerator g) -> newMetric g
    where newMetric g = let (val, gen) = runGenerator g in
                            m { metricValue = val
                              , metricValueGen = ValueGenerator gen}
