module Hmond.Generators ( fixedGenerator
                        , decrementingGenerator
                        ) where

import Hmond.Types

data FixedGenerator = FixedGenerator Int
    deriving Show


instance ValueGenerator_ FixedGenerator where
    runGenerator f@(FixedGenerator i) = (i, f)


fixedGenerator :: Int -> ValueGenerator
fixedGenerator = ValueGenerator . FixedGenerator



data DecrementingGenerator = DecrementingGenerator Int
    deriving Show


instance ValueGenerator_ DecrementingGenerator where
    runGenerator (DecrementingGenerator i) = (i, (DecrementingGenerator (i - 1)))


decrementingGenerator :: Int -> ValueGenerator
decrementingGenerator = ValueGenerator . DecrementingGenerator
