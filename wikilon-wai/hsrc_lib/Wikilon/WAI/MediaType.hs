{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.MediaType
    ( mediaTypeAODict
    , mediaTypeAODef
    , mediaTypeABC
    , mediaTypeTextPlain
    , mediaTypeTextHTML
    , mediaTypeTextCSV
    , mediaTypeFormURLEncoded
    , mediaTypeMultiPartFormData
    , mediaTypeGzip
    ) where

import qualified Network.HTTP.Media as HTTP

mediaTypeAODict :: HTTP.MediaType 
mediaTypeAODict = "text/vnd.org.awelon.aodict"

mediaTypeAODef :: HTTP.MediaType
mediaTypeAODef = "text/vnd.org.awelon.aodef"

mediaTypeABC :: HTTP.MediaType
mediaTypeABC = "application/vnd.org.awelon.abc"

mediaTypeTextPlain :: HTTP.MediaType
mediaTypeTextPlain = "text/plain"

mediaTypeTextHTML :: HTTP.MediaType
mediaTypeTextHTML = "text/html"

mediaTypeTextCSV :: HTTP.MediaType
mediaTypeTextCSV = "text/csv"

mediaTypeFormURLEncoded :: HTTP.MediaType
mediaTypeFormURLEncoded = "application/x-www-form-urlencoded"

mediaTypeMultiPartFormData :: HTTP.MediaType
mediaTypeMultiPartFormData = "multipart/form-data"

mediaTypeGzip :: HTTP.MediaType
mediaTypeGzip = "application/gzip"
