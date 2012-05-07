module Hmond.Metrics ( metrics
                     , runMetrics
                     ) where

import System.Random (StdGen)

import Hmond.Types
import Hmond.Generators


-- XXX Ensure that the same StdGen isn't used to seed multiple metric
-- generators.
metrics :: StdGen -> [Metric]
metrics gen = [ makeMetric fixedGenerator (MtInt32 1) "fixed int"
              , makeMetric fixedGenerator (MtDouble 1.5) "fixed double"
              , makeMetric decrementingGenerator (MtInt32 10) "decr int"
              , makeMetric decrementingGenerator (MtDouble 10.5) "decr double"
              , makeMetric fixedGenerator (MtString "Bob") "fixed string"
              , makeMetric caseTogglingGenerator (MtString "bob") "case toggling string"
              , makeMetric (randGenerator gen) (MtInt32 1) "rand int"
              ]


runMetrics ::  Host -> Host
runMetrics host = host { hostMetrics = newMetrics}
    where newMetrics = map runMetric $ hostMetrics host
          runMetric (MkM m fn) = fn m
