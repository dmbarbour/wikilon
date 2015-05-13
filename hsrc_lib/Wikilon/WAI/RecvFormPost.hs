{-# LANGUAGE OverloadedStrings #-}
-- | specific utility for receiving Form Post messages
-- i.e. using multipart\/form-data or application\/x-www-url-encoded
-- 
-- currently just a thin wrapper around Network.Wai.Parse.parseRequestBody
module Wikilon.WAI.RecvFormPost
    ( recvFormPost
    , PostParams
    , PostParam
    , FileInfo(..)
    , postParamContent
    , postParamContentUnzip
    , parseRequestPostParams
    ) where

import Control.Arrow (second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Codec.Compression.GZip as GZip
import Wikilon.WAI.Utils

type PostParams = [(BS.ByteString, PostParam)]
type PostParam = Either FileInfo LBS.ByteString 
data FileInfo = FileInfo
    { fileName :: BS.ByteString
    , fileContentType :: BS.ByteString
    , fileContent :: LBS.ByteString
    }

-- | basic extract bytestring from a post param
postParamContent :: PostParam -> LBS.ByteString
postParamContent = either fileContent id 

-- | extendeded variation of postParamContentUnzip that can 
-- heuristically handle gzipped files:
--   .gz extension 
--   application\/gzip
--   application\/x-gzip
postParamContentUnzip :: PostParam -> LBS.ByteString
postParamContentUnzip = either fromFile id where
    fromFile f = 
        let ungz = if gzipped f then GZip.decompress else id in
        ungz $ fileContent f
    gzipped f = ("gzip" `BS.isSuffixOf` fileContentType f)
        || (".gz" `BS.isSuffixOf` fileName f)

recvFormPost :: (PostParams -> WikilonApp) -> WikilonApp
recvFormPost appWithParams = app where
    app = branchOnInputMedia -- validate the two common post types.
        [(mediaTypeFormURLEncoded, appParsed)
        ,(mediaTypeMultiPartFormData, appParsed)
        ]
    appParsed w cap rq k =
        parseRequestPostParams rq >>= \ pps ->
        appWithParams pps w cap rq k

parseRequestPostParams :: Wai.Request -> IO PostParams
parseRequestPostParams rq =
    Wai.parseRequestBody Wai.lbsBackEnd rq >>= \ (ps,fs) ->
    let ps' = fmap (second (Right . LBS.fromStrict)) ps in
    let fs' = fmap (second (Left . fromWaiFileInfo)) fs in
    return (ps' ++ fs')

-- trivial conversion
fromWaiFileInfo :: Wai.FileInfo LBS.ByteString -> FileInfo
fromWaiFileInfo wfi = FileInfo
    { fileName = Wai.fileName wfi
    , fileContentType = Wai.fileContentType wfi
    , fileContent = Wai.fileContent wfi
    }
