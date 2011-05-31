module Hmond.ValueGenerator ( evalGenerator
                            , execGenerator
                            ) where

import Hmond.Types


evalGenerator :: ValueGenerator_ g => g -> Int
evalGenerator = fst . runGenerator


execGenerator :: ValueGenerator_ g => g -> g
execGenerator = snd . runGenerator
