module Hmond.Hosts ( hosts
                   , runHosts
                   ) where


import Hmond.Types
import Hmond.Metrics


hosts :: [Host]
hosts = [ Host "comp00.vm.concurrent-thinking.com" "192.168.1.1" metrics
        , Host "comp01.vm.concurrent-thinking.com" "192.168.1.2" metrics
        ]


runHosts :: [Host] -> [Host]
runHosts = map runMetrics
