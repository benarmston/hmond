{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.GangliaXml.Attributes
    ( source
    , version
    , localtime
    , owner
    , latlong
    , url
    , name
    , reported
    , tn
    , tmax
    , dmax
    , _type
    , val
    , units
    , ip
    ) where

import Prelude ()

import Text.Blaze.Internal (Attribute, AttributeValue, attribute)

source :: AttributeValue -> Attribute
source = attribute "source" " SOURCE=\""
{-# INLINE source #-}

version :: AttributeValue -> Attribute
version = attribute "version" " VERSION=\""
{-# INLINE version #-}

localtime :: AttributeValue -> Attribute
localtime = attribute "localtime" " LOCALTIME=\""
{-# INLINE localtime #-}

owner :: AttributeValue -> Attribute
owner = attribute "owner" " OWNER=\""
{-# INLINE owner #-}

latlong :: AttributeValue -> Attribute
latlong = attribute "latlong" " LATLONG=\""
{-# INLINE latlong #-}

url :: AttributeValue -> Attribute
url = attribute "url" " URL=\""
{-# INLINE url #-}

name :: AttributeValue -> Attribute
name = attribute "name" " NAME=\""
{-# INLINE name #-}

reported :: AttributeValue -> Attribute
reported = attribute "reported" " REPORTED=\""
{-# INLINE reported #-}

tn :: AttributeValue -> Attribute
tn = attribute "tn" " TN=\""
{-# INLINE tn #-}

tmax :: AttributeValue -> Attribute
tmax = attribute "tmax" " TMAX=\""
{-# INLINE tmax #-}

dmax :: AttributeValue -> Attribute
dmax = attribute "dmax" " DMAX=\""
{-# INLINE dmax #-}

_type :: AttributeValue -> Attribute
_type = attribute "_type" " _TYPE=\""
{-# INLINE _type #-}

val :: AttributeValue -> Attribute
val = attribute "val" " VAL=\""
{-# INLINE val #-}

units :: AttributeValue -> Attribute
units = attribute "units" " UNITS=\""
{-# INLINE units #-}

slope :: AttributeValue -> Attribute
slope = attribute "slope" " SLOPE=\""
{-# INLINE slope #-}

ip :: AttributeValue -> Attribute
ip = attribute "ip" " IP=\""
{-# INLINE ip #-}
