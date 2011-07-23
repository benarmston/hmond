{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Hmond.Options (Options
                     ,options
                     ,optionsConf) where

import System.Console.CmdArgs

data Options = Options
  { conf      :: FilePath
  } deriving (Show, Data, Typeable)

options :: Annotate Ann
options = record Options{} [conf := "hmond.cfg" += typFile += help "Use config file FILE"]
          += program "hmond"
          += summary "Hmond Pseudo Ganglia Monitor (C) Ben Armston 2011"
          += help "A pseudo ganglia monitor providing faked metric values"
          += helpArg [name "h"]
          += versionArg [summary "Hmond version 0.1"]

optionsConf :: Options -> FilePath
optionsConf = conf
