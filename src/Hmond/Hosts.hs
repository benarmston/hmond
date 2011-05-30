module Hmond.Hosts ( hosts
                   ) where


import Hmond.Types


hosts :: [Host]
hosts = [ Host "comp00.vm.concurrent-thinking.com" "192.168.1.1"
        , Host "comp01.vm.concurrent-thinking.com" "192.168.1.2"
        ]
