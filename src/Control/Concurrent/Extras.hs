module Control.Concurrent.Extras (
    withPeriodicity
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)


withPeriodicity :: NominalDiffTime -> IO () -> IO ()
withPeriodicity period action = do
    forever $ do
        t0 <- getCurrentTime
        action
        t1 <- getCurrentTime
        let sleep = period - (diffUTCTime t0 t1)
        unless (sleep < 0) $ threadDelay (ndt2usec sleep)


ndt2usec :: NominalDiffTime -> Int
ndt2usec dt = truncate $ (realToFrac dt) * (1000000 :: Double)
