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
runMetric m@(Metric _ _ (ValueGenerator g)) = let g' = execGenerator g in
                                                  (m, m {metricValueGen = ValueGenerator g'})


metricValue :: Metric -> Int
metricValue (Metric _ _ (ValueGenerator g)) = evalGenerator g
