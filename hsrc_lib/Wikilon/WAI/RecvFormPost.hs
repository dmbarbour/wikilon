
-- | specific utility for receiving Form Post messages
-- i.e. using multipart\/form-data or application\/x-www-url-encoded
-- 
-- currently just a thin wrapper around Network.Wai.Parse.parseRequestBody
module Wikilon.WAI.RecvFormPost
    ( recvFormPost
    , PostParam(..)
    , FileInfo(..)
    , postParamContent
    ) where

import Control.Arrow (second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai.Parse as Wai
import Wikilon.WAI.Utils

type PostParam = Either FileInfo LBS.ByteString 
data FileInfo = FileInfo
    { fileName :: BS.ByteString
    , fileContentType :: BS.ByteString
    , fileContent :: LBS.ByteString
    }

postParamContent :: PostParam -> LBS.ByteString
postParamContent = either fileContent id 

recvFormPost :: ([(BS.ByteString, PostParam)] -> WikilonApp) -> WikilonApp
recvFormPost appWithParams = app where
    app = branchOnInputMedia -- validate the two common post types.
        [(mediaTypeFormURLEncoded, appParsed)
        ,(mediaTypeMultiPartFormData, appParsed)
        ]
    appParsed w cap rq k =
        Wai.parseRequestBody Wai.lbsBackEnd rq >>= \ (ps,fs) ->
        let ps' = fmap (second (Right . LBS.fromStrict)) ps in
        let fs' = fmap (second (Left . fromWaiFileInfo)) fs in
        appWithParams (ps' ++ fs') w cap rq k

-- trivial conversion
fromWaiFileInfo :: Wai.FileInfo LBS.ByteString -> FileInfo
fromWaiFileInfo wfi = FileInfo
    { fileName = Wai.fileName wfi
    , fileContentType = Wai.fileContentType wfi
    , fileContent = Wai.fileContent wfi
    }
