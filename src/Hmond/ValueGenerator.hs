module Hmond.ValueGenerator ( evalGenerator
                            , execGenerator
                            ) where

import Hmond.Types


evalGenerator :: ValueGenerator -> Int
evalGenerator = fst . runGenerator


execGenerator :: ValueGenerator -> ValueGenerator
execGenerator = snd . runGenerator
