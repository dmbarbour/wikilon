{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | specific utility for receiving Form Post messages
-- i.e. using multipart\/form-data or application\/x-www-url-encoded
-- 
-- currently just a thin wrapper around Network.Wai.Parse.parseRequestBody
module Wikilon.WAI.RecvFormPost
    ( recvFormPost
    , PostParams
    , PostParam
    , FileInfo(..)
    , getPostParam
    , getPostParamUnzip
    , postParamContent
    , postParamContentUnzip
    , parseRequestPostParams
    , normalizeNewlines
    ) where

import Control.Arrow (second)
import Data.Monoid
import qualified Data.List as L
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

getPostParam :: BS.ByteString -> PostParams -> Maybe LBS.ByteString
getPostParam s = fmap postParamContent . L.lookup s

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

getPostParamUnzip :: BS.ByteString -> PostParams -> Maybe LBS.ByteString
getPostParamUnzip s = fmap postParamContentUnzip . L.lookup s

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
    let ps' = fmap (second (Right . normalizeNewlines . LBS.fromStrict)) ps in
    let fs' = fmap (second (Left . fromWaiFileInfo)) fs in
    return (ps' ++ fs')

-- | Normalize newlines will translate CRLF pairs to just LF.
--
-- note: CR on its own will be left alone.
--
-- Unfortunately, this may damage otherwise valid ABC texts that
-- contain CRLF endings internally. However, I'd rather normalize
-- and simply discourage use of text that uses any C0 or C1 chars
-- other than LF.
--
-- 
-- 
normalizeNewlines :: LBS.ByteString -> LBS.ByteString
normalizeNewlines s0 = nnl 0 s0 where
    nnl !n s = case LBS.elemIndex 13 s of
        Nothing -> s0
        Just ix -> 
            let s' = LBS.drop (ix + 1) s in
            case LBS.uncons s' of
                Just (10, _) -> LBS.take (n + ix) s0 <> normalizeNewlines s'
                _ -> nnl (n + ix + 1) s'

-- trivial conversion
fromWaiFileInfo :: Wai.FileInfo LBS.ByteString -> FileInfo
fromWaiFileInfo wfi = FileInfo
    { fileName = Wai.fileName wfi
    , fileContentType = Wai.fileContentType wfi
    , fileContent = Wai.fileContent wfi
    }
