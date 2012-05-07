module Hmond.Hosts ( hosts
                   , runHosts
                   ) where


import System.Random (StdGen)

import Hmond.Types
import Hmond.Metrics


-- XXX Ensure that the same StdGen isn't used to seed multiple hosts's
-- metrics.
hosts :: StdGen -> [Host]
hosts gen = [ Host "comp00.vm.concurrent-thinking.com" "192.168.1.1" metrics'
            , Host "comp01.vm.concurrent-thinking.com" "192.168.1.2" metrics'
            ]
    where metrics' = metrics gen


runHosts :: [Host] -> [Host]
runHosts = map runMetrics
