{-# LANGUAGE GADTs #-}

module Hmond.Generators where

import Data.Char (isUpper, toLower, toUpper)
import Data.Int (Int32)
import System.Random (StdGen, next)

import Hmond.Types

fixedGenerator :: MetricType a -> ValueGenerator a
fixedGenerator i = ValueGenerator fixedVal
    where fixedVal = (i, fixedGenerator i)


decrementingGenerator :: Num a => MetricType a -> ValueGenerator a
decrementingGenerator (MtInt8 i) = ValueGenerator decrement
    where decrement = (MtInt8 i, decrementingGenerator $ MtInt8 (i - 1))

decrementingGenerator (MtInt16 i) = ValueGenerator decrement
    where decrement = (MtInt16 i, decrementingGenerator $ MtInt16 (i - 1))

decrementingGenerator (MtInt32 i) = ValueGenerator decrement
    where decrement = (MtInt32 i, decrementingGenerator $ MtInt32 (i - 1))

decrementingGenerator (MtUInt8 i) = ValueGenerator decrement
    where decrement = (MtUInt8 i, decrementingGenerator $ MtUInt8 (i - 1))

decrementingGenerator (MtUInt16 i) = ValueGenerator decrement
    where decrement = (MtUInt16 i, decrementingGenerator $ MtUInt16 (i - 1))

decrementingGenerator (MtUInt32 i) = ValueGenerator decrement
    where decrement = (MtUInt32 i, decrementingGenerator $ MtUInt32 (i - 1))

decrementingGenerator (MtFloat i) = ValueGenerator decrement
    where decrement = (MtFloat i, decrementingGenerator $ MtFloat (i - 1))

decrementingGenerator (MtDouble i) = ValueGenerator decrement
    where decrement = (MtDouble i, decrementingGenerator $ MtDouble (i - 1))

decrementingGenerator _ = error "Type system prevents us from getting here"


caseTogglingGenerator :: MetricType String -> ValueGenerator String
caseTogglingGenerator (MtString s) = ValueGenerator (MtString s, caseTogglingGenerator toggled)
    where toggled = MtString $ if all isUpper s
                                then map toLower s
                                else map toUpper s


randGenerator :: StdGen -> MetricType Int32 -> ValueGenerator Int32
randGenerator gen mt = ValueGenerator random
    where random = (mt, randGenerator gen' $ MtInt32 (fromIntegral i'))
          (i', gen') = next gen
