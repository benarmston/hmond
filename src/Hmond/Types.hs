{-# LANGUAGE ExistentialQuantification #-}

module Hmond.Types ( Host(..)
                    ) where


data Host = Host { hostname    :: String
                 , hostIP      :: String
                 } deriving Show
