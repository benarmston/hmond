module Main where

import Data.Char

-- | Types

data ValueGenerator a = ValueGenerator { runGenerator :: (a, ValueGenerator a) }


-- | particular generators
--
fixedGenerator :: a -> ValueGenerator a
fixedGenerator i = ValueGenerator fixedVal
    where fixedVal = (i, fixedGenerator i)


decrementingGenerator :: Num a => a -> ValueGenerator a
decrementingGenerator i = ValueGenerator decrement
    where decrement = (i, decrementingGenerator (i - 1))


caseTogglingGenerator :: String -> ValueGenerator String
caseTogglingGenerator s = ValueGenerator (s, caseTogglingGenerator toggled)
    where toggled = if all isUpper s
                      then map toLower s
                      else map toUpper s


-- | generator convenience functions
--
evalGenerator :: ValueGenerator a -> a
evalGenerator = fst . runGenerator


execGenerator :: ValueGenerator a -> ValueGenerator a
execGenerator = snd . runGenerator




main :: IO ()
main = do
    let fgi = fixedGenerator 10
        dgi = decrementingGenerator 10
        dgf = decrementingGenerator 10.5
        fgs = fixedGenerator "Bob"
        ctg = caseTogglingGenerator "bob"
    printVals fgi dgi dgf fgs ctg


printVals ::  (Show a, Show b, Show c, Show d, Show e) =>
              ValueGenerator a ->
              ValueGenerator b ->
              ValueGenerator c ->
              ValueGenerator d ->
              ValueGenerator e -> IO ()
printVals a b c d e = do
    putStrLn $ "a: " ++ show av ++ " b: " ++ show bv ++ " c: " ++ show cv ++ " d: " ++ show dv ++ " e: " ++ show ev
    printVals a' b' c' d' e'
  where
    (av, a') = runGenerator a
    (bv, b') = runGenerator b
    (cv, c') = runGenerator c
    (dv, d') = runGenerator d
    (ev, e') = runGenerator e
