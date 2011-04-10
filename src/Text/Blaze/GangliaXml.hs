{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.GangliaXml
    ( module Text.Blaze
    , ganglia_xml
    , cluster
    , host
    , metric
    ) where

import Prelude ()

import Text.Blaze
import Text.Blaze.Internal

ganglia_xml :: Html -> Html
ganglia_xml = Parent "ganglia_xml" "<GANGLIA_XML" "</GANGLIA_XML>"

cluster :: Html -> Html
cluster = Parent "cluster" "<CLUSTER" "</CLUSTER>"

host :: Html -> Html
host = Parent "host" "<HOST" "</HOST>"
{-# INLINE host #-}

metric :: Html
metric = Leaf "metric" "<METRIC" "/>"
{-# INLINE metric #-}
