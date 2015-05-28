{-# LANGUAGE OverloadedStrings #-}

-- | miscellaneous utility functions to create web services
module Wikilon.WAI.Utils
    -- MIDDLEWARE 
    ( routeOnMethod
    , routeOnMethod'
    , justGET
    , branchOnInputMedia
    , branchOnInputMedia'
    , branchOnOutputMedia
    , branchOnOutputMedia'
    , waiMiddleware
    , enableGzipEncoding

    -- APPS
    , defaultRouteOnMethod
    , toBeImplementedLater
    , basicWebPage

    -- RESPONSES
    , htmlResponse
    , okNoContent

    , eNotAllowed
    , msgOptions
    , eNotAcceptable
    , eUnsupportedMediaType
    , eNotFound
    , eBadName
    , eBadRequest
    , eServerError




    -- HEADERS
    , textHtml
    , plainText
    , noCache
    , eTagN, eTagNW

    -- HTML
    , HTML
    , statusToTitle
    , htmlTime, htmlSimpTime, htmlWeekTime
    , renderHTML

    , htmlMetaNoIndex
    , htmlMetaCharsetUtf8
    , htmlUseDefaultCSS
    , htmlUseDefaultFavicon
    , htmlWikilonBase
    , htmlHeaderCommon

    -- MEDIA
    , mediaTypeAODict
    , mediaTypeTextPlain
    , mediaTypeTextHTML
    , mediaTypeTextCSV
    , mediaTypeFormURLEncoded
    , mediaTypeMultiPartFormData
    , mediaTypeGzip

    , module Wikilon.WAI.Types
    ) where

--import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe (listToMaybe)
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
-- import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as Wai
import Wikilon.Dict.Word (listWordConstraintsForHumans)
import Wikilon.WAI.Types
import Wikilon.Root
import qualified Wikilon.Time as Time

-- | Route based on method. Also provides reasonable default
-- implementations for OPTIONS and HEAD.
routeOnMethod :: [(HTTP.Method, WikilonApp)] -> WikilonApp
routeOnMethod lms = routeOnMethod' lms (defaultRouteOnMethod lms)

-- | route on methods, with a fallback if none of the methods match
routeOnMethod' :: [(HTTP.Method, WikilonApp)] -> WikilonApp -> WikilonApp
routeOnMethod' lms def w cap rq k =
    case L.lookup (Wai.requestMethod rq) lms of
        Just app -> app w cap rq k
        Nothing -> def w cap rq k
{-# INLINE routeOnMethod' #-}

waiMiddleware :: Wai.Middleware -> Middleware
waiMiddleware mw app w cap = mw (app w cap)
{-# INLINE waiMiddleware #-}

enableGzipEncoding :: Middleware
enableGzipEncoding = waiMiddleware (Wai.gzip Wai.def)

-- | Application route that only supports 'GET'
justGET :: WikilonApp -> WikilonApp
justGET app = routeOnMethod [(HTTP.methodGet, app)]

-- | Fallback behavior after every entry in the method table has failed.
-- This provides default implementations for OPTIONS and HEAD.
defaultRouteOnMethod :: [(HTTP.Method, WikilonApp)] -> WikilonApp
defaultRouteOnMethod lms w cap rq k = body where
    m = Wai.requestMethod rq
    body = 
        if (m == HTTP.methodOptions) then options else
        if (m == HTTP.methodHead) then tryHead else
        notAllowed
    tryHead = case L.lookup HTTP.methodGet lms of
        Nothing -> notAllowed -- no GET option
        Just get -> get w cap rq $ \ response ->
            let status = Wai.responseStatus response in
            let headers = Wai.responseHeaders response in
            k $ Wai.responseLBS status headers (LBS.empty)
    notAllowed = k $ eNotAllowed (fst <$> lms)
    options = k $ msgOptions (fst <$> lms)

eNotAllowed, msgOptions :: [HTTP.Method] -> Wai.Response

eNotAllowed methods = 
    let status = HTTP.methodNotAllowed405 in
    let headers = [allow methods, textHtml, noCache] in
    let title = statusToTitle status in
    Wai.responseLBS status headers $ renderHTML $ do 
    H.head $ do
        htmlMetaCharsetUtf8
        htmlMetaNoIndex
        H.title title
    H.body $ do
        H.h1 title
        bodyMethodsAllowed methods

msgOptions methods =
    let status = HTTP.ok200 in
    let headers = [allow methods, textHtml] in
    let title = statusToTitle status in
    Wai.responseLBS status headers $ renderHTML $ do
    H.head $ do
        htmlMetaCharsetUtf8
        H.title title
    H.body $ do
        H.h1 title
        bodyMethodsAllowed methods

allow :: [HTTP.Method] -> HTTP.Header
allow methods = ("Allow", BS.intercalate ", " methods)

bodyMethodsAllowed :: [HTTP.Method] -> HTML
bodyMethodsAllowed methods = do
    H.p "Methods specifically implemented for this resource: "
    H.ul $ mapM_ (H.li . H.unsafeByteString) methods
    H.p "HEAD and OPTIONS may have default implementations."

-- | Select an application based on a preferred media output. This
-- is mostly for GET requests. Branching on input content type for
-- PUT or POST will require a separate function.
branchOnOutputMedia :: [(HTTP.MediaType, WikilonApp)] -> WikilonApp
branchOnOutputMedia lms = branchOnOutputMedia' lms $ \ _w _cap rq k ->
    k $ eNotAcceptable rq (fst <$> lms)

-- somewhat ad-hoc for now...
-- should I have different error media types? not sure.
eNotAcceptable :: Wai.Request -> [HTTP.MediaType] -> Wai.Response
eNotAcceptable rq mediaTypes = 
    let status = HTTP.notAcceptable406 in
    let headers = [noCache, textHtml] in
    let title = statusToTitle status in
    Wai.responseLBS status headers $ renderHTML $ do
    H.head $ do
        htmlMetaCharsetUtf8
        htmlMetaNoIndex
        H.title title
    H.body $ do
        H.h1 title
        H.p $ do 
            H.string "Requested Media Types: " 
            case L.lookup HTTP.hAccept (Wai.requestHeaders rq) of
                Nothing -> "*/*" 
                Just bs -> H.string $ UTF8.toString bs
        H.p "Available media types for this resource: "
        H.ul $ mapM_ (H.li . H.string . show) mediaTypes  
        H.p "Also, the library parsing Accept headers is picky.\n\
               \Include quality before other params (cf RFC2616 sec14)."

-- | Select a media type based on preferred media output, with a
-- fallback behavior on 406 Not Acceptable. 
branchOnOutputMedia' :: [(HTTP.MediaType, WikilonApp)] -> WikilonApp -> WikilonApp
branchOnOutputMedia' lms e406 w cap rq k =
    let app0 = maybe e406 snd $ listToMaybe lms in -- first in list
    case L.lookup HTTP.hAccept (Wai.requestHeaders rq) of
        Nothing -> app0 w cap rq k -- no client preference
        Just hdrAccept -> case HTTP.mapAcceptMedia lms hdrAccept of
            Nothing -> e406 w cap rq k
            Just app -> app w cap rq k

-- | Select an application based on input types, fallback to error 415.
branchOnInputMedia :: [(HTTP.MediaType, WikilonApp)] -> WikilonApp
branchOnInputMedia lms = branchOnInputMedia' lms $ \ _w _cap rq k ->
    k $ eUnsupportedMediaType rq (fst <$> lms)

eUnsupportedMediaType :: Wai.Request -> [HTTP.MediaType] -> Wai.Response
eUnsupportedMediaType rq mediaTypes = 
    let status = HTTP.unsupportedMediaType415 in
    let headers = [noCache, textHtml] in
    let title = statusToTitle status in
    Wai.responseLBS status headers $ renderHTML $ do
    H.head $ do
        htmlMetaCharsetUtf8
        htmlMetaNoIndex
        H.title title
    H.body $ do
        H.h1 title
        H.p $ do
            H.string "Provided Content-Type: "
            case L.lookup HTTP.hContentType (Wai.requestHeaders rq) of
                Nothing -> H.string "(undefined)"
                Just bs -> H.string $ UTF8.toString bs
        H.p "Acceptable media types for this resource: "
        H.ul $ mapM_ (H.li . H.string . show) mediaTypes

-- | select based on the Content-Type field of  media, with a fallback in case no media matches
branchOnInputMedia' :: [(HTTP.MediaType, WikilonApp)] -> WikilonApp -> WikilonApp
branchOnInputMedia' lms e415 w cap rq k =
    case L.lookup HTTP.hContentType (Wai.requestHeaders rq) of
        Nothing -> e415 w cap rq k -- no media type specified
        Just ctype -> 
            let app = maybe e415 id $ HTTP.mapContentMedia lms ctype in
            app w cap rq k

-- | Our basic 404 Not Found page. 
eNotFound :: Wai.Request -> Wai.Response
eNotFound rq = 
    let status = HTTP.notFound404 in
    let headers = [noCache, textHtml] in
    let title = statusToTitle status in
    Wai.responseLBS status headers $ renderHTML $ do
    H.head $ do
        htmlMetaCharsetUtf8
        htmlMetaNoIndex
        H.title title
    H.body $ do
        H.h1 title
        H.p $ do
            H.string "Requested URI: "
            H.string $ UTF8.toString $ HTTP.urlDecode False $ Wai.rawPathInfo rq
        H.p "Requested URI is unknown to the Wikilon server."

-- | 404 illegal URI (could not capture dictionary, word, or user name)
eBadName :: Captures -> Wai.Response
eBadName caps = 
    let status = HTTP.notFound404 in
    let headers = [textHtml, noCache] in
    Wai.responseLBS status headers $ renderHTML $ do
        let title = "404 Illegal URI"
        H.head $ do
            htmlMetaCharsetUtf8
            htmlMetaNoIndex
            H.title title
        H.body $ do
            H.h1 title
            H.p "Name used in a client-constructed URI is invalid by Wikilon's\n\
                \heuristic constraints. Dictionary, word, tokens, and user names\n\
                \must obey the following rules:\n\
                \"
            H.ul $ mapM_ (H.li . H.string) listWordConstraintsForHumans
            H.p "These constraints are intended to make names more friendly in\n\
                \contexts of URIs, HTML, CSV, and documentation text. Any UTF-8\n\
                \in a URI must additionally be %-encoded, but good browsers will\n\
                \display it nicely.\n\
                \"
            H.h2 "Captures"
            H.p "Captured name(s) for this URI:"
            H.p $ H.string $ show caps

-- | generic response for bad requests. 
eBadRequest :: String -> Wai.Response
eBadRequest msg =
    let status = HTTP.badRequest400 in
    let headers = [noCache, textHtml] in
    let title = statusToTitle status in
    Wai.responseLBS status headers $ renderHTML $ do
    H.head $ do
        htmlMetaCharsetUtf8
        htmlMetaNoIndex
        H.title title
    H.body $ do
        H.h1 title
        H.p $ H.string msg

-- | generic server failure should not contain any sensitive 
-- information (for security reasons). Use with static strings.
--
-- TODO: log the requests somewhere for the admin
eServerError :: String -> Wai.Response
eServerError msg =
    let status = HTTP.status500 in
    let headers = [noCache, textHtml] in
    let title = statusToTitle status in
    Wai.responseLBS status headers $ renderHTML $ do
    H.head $ do
        htmlMetaCharsetUtf8
        htmlMetaNoIndex
        H.title title
    H.body $ do
        H.h1 title
        H.p $ H.string msg


-- | placeholder for applications I want to implement later...
toBeImplementedLater :: String -> WikilonApp
toBeImplementedLater msg _ _ _ k = k $ Wai.responseLBS HTTP.status202 [textHtml,noCache] $ 
    renderHTML $ do 
    H.head $ do 
        htmlMetaCharsetUtf8
        htmlMetaNoIndex
        H.title $ H.string $ "TODO: " ++ msg
    H.body $ do
        H.h1 "TODO"
        H.p $ H.string msg

-- | a web application that just serves one HTML page
basicWebPage :: (Wikilon -> Captures -> Wai.Request -> HTML) -> WikilonApp
basicWebPage htmlDoc = app where
    app = justGET $ branchOnOutputMedia [(mediaTypeTextHTML, getPage)]
    getPage w cap rq k = k $ Wai.responseLBS HTTP.ok200 [textHtml] $ 
        renderHTML $ htmlDoc w cap rq

-- | basic HTML response
htmlResponse :: HTTP.Status -> HTML -> Wai.Response
htmlResponse status = Wai.responseLBS status [textHtml] . renderHTML

okNoContent :: Wai.Response
okNoContent = Wai.responseLBS HTTP.noContent204 [] LBS.empty

-- | Content-Type: text\/html; charset=utf-8
textHtml :: HTTP.Header
textHtml = (HTTP.hContentType, "text/html; charset=utf-8")

-- | Content-Type: text\/plain; charset=utf-8
plainText :: HTTP.Header
plainText = (HTTP.hContentType,"text/plain; charset=utf-8")

-- | Cache-Control: no-cache
noCache :: HTTP.Header
noCache = (HTTP.hCacheControl, "no-cache")


-- | ETag: number  (weak or strong)
eTagN, eTagNW :: (Integral n) => n -> HTTP.Header
eTagN n = ("ETag", UTF8.fromString $ show (show (toInteger n)))
eTagNW n = ("ETag", UTF8.fromString $ "W/" ++ show (show (toInteger n)))

-- | since I'm not fond of blaze-html's mixed-case abbreviations...
type HTML = H.Html

-- | Render HTML5 to a lazy bytestring. This also adds the outer
-- \<html\> tags and the doctype. Always renders to UTF-8. 
renderHTML :: HTML -> LazyUTF8.ByteString
renderHTML = Text.Blaze.Html.Renderer.Utf8.renderHtml . H.docTypeHtml
{-# INLINE renderHTML #-}

-- | Render time for both humans and machines.
htmlTime :: String -> Time.T -> HTML
htmlTime sFormat tm = 
    let dt = H.stringValue $ show tm in
    let htm = H.string $ Time.formatTime sFormat tm in
    H.time ! A.datetime dt $ htm

htmlWeekTime :: Time.T -> HTML
htmlWeekTime = htmlTime "%a %R"

htmlSimpTime :: Time.T -> HTML
htmlSimpTime = htmlTime "%Y %b %e %R"

-- | A meta element (under \<head\>) to indicate a page's content
-- should not be indexed or followed. This is, of course, entirely
-- discretionary for the robots.
htmlMetaNoIndex :: HTML
htmlMetaNoIndex = H.meta ! A.name "robots" ! A.content "noindex, nofollow"

-- | Indicate charset utf-8 redundantly in html content    
htmlMetaCharsetUtf8 :: HTML
htmlMetaCharsetUtf8 = H.meta ! A.charset "UTF-8"

-- | The default CSS file will be available as css and css\/default
-- actual css content will be left to the master dictionary
htmlUseDefaultCSS :: HTML
htmlUseDefaultCSS = 
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "css" 

-- | I don't actually have any favicons at the moment, and I'll
-- likely leave this to the master dictionary.
htmlUseDefaultFavicon :: HTML
htmlUseDefaultFavicon =
    H.link ! A.rel "icons" ! A.type_ "image/png" ! A.href "favicon"

-- | Rather than use wikilon_httpRoot as an absolute route in every URI, 
-- I plan to use the \<base\> tag once in the header and use relative 
-- routes to the wikilon_httpRoot. The disadvantage here is that all
-- # anchors must use the same relative paths. 
htmlWikilonBase :: Wikilon -> HTML
htmlWikilonBase w =
    let baseVal = H.unsafeByteStringValue $ finiSlash $ wikilon_httpRoot w in
    H.base ! A.href baseVal

finiSlash :: BS.ByteString -> BS.ByteString
finiSlash s = case BS.unsnoc s of
    Just (_, 47) -> s
    _ -> s <> "/"

-- | utf8, base, css, favicon
htmlHeaderCommon :: Wikilon -> HTML
htmlHeaderCommon w = do
    htmlMetaCharsetUtf8
    htmlWikilonBase w
    htmlUseDefaultCSS
    htmlUseDefaultFavicon 

-- | e.g. HTTP.status405 becomes "405 Method Not Allowed" suitable for
-- a header or title.
statusToTitle :: HTTP.Status -> HTML
statusToTitle status =
    H.toMarkup (HTTP.statusCode status) <> " " <> 
    H.unsafeByteString (HTTP.statusMessage status)

mediaTypeAODict :: HTTP.MediaType 
mediaTypeAODict = "text/vnd.org.awelon.aodict"

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

