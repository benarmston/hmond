module Main where

import Network
import System.Posix

import Hmond.Server (start)

main :: IO ()
main = withSocketsDo $ do
    installHandler sigPIPE Ignore Nothing
    start
