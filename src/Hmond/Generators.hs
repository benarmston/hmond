module Hmond.Generators ( fixedGenerator
                        , decrementingGenerator
                        ) where

import Hmond.Types

fixedGenerator :: Int -> ValueGenerator
fixedGenerator i = ValueGenerator fixedVal
    where fixedVal = (i, fixedGenerator i)


decrementingGenerator :: Int -> ValueGenerator
decrementingGenerator i = ValueGenerator decrement
    where decrement = (i, decrementingGenerator (i - 1))
