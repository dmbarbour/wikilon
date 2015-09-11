{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

-- | Pages for the toplevel list-of-dictionaries
--
-- TODO: 
--   rename dictionaries
--   fork dictionaries
--   merge dictionaries    
module Wikilon.WAI.Pages.DictList
    ( allDictionaries
    , dictList
    , dictCreate
    , formSimpleCreateDict
    , ppDictName
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai

import qualified Wikilon.Dict as Dict
import Wikilon.WAI.Utils
import Wikilon.WAI.RecvFormPost
import Wikilon.WAI.Routes
import qualified Wikilon.WAI.RegexPatterns as Regex

-- | Okay, so our first resource is a collection of dictionaries.
-- There aren't any whole-collection updates at this layer, but
-- there is some ability to browse a collection.
allDictionaries :: WikilonApp
allDictionaries = app where
    app = justGET onGet
    onGet = branchOnOutputMedia
        [(mediaTypeTextHTML, listOfDictsPage)
        ,(mediaTypeTextPlain, listOfDictsText)
--      ,(mediaTypeTextCSV, listOfDictsCSV)
        ]

-- | restricted page for a textual list of dictionaries
dictList :: WikilonApp
dictList = justGET $ branchOnOutputMedia 
    [(mediaTypeTextPlain, listOfDictsText)]

-- | simply list dictionaries by name, one per line.
listOfDictsText :: WikilonApp
listOfDictsText w _cap _rq k = join $ 
    wikilon_action w listBranches >>= \ lBranchNames -> 
    return $ k $ Wai.responseLBS HTTP.ok200 [plainText] $ BB.toLazyByteString $ 
        let encName n = BB.byteString (wordToUTF8 n) <> BB.charUtf8 '\n' in
        mconcat (encName <$> lBranchNames)

-- | Our list of dictionaries is another page that should be configured
-- by our clients. It will need a lot of features:
--
--   list existing dictionaries (perhaps by activity) and link to them
--   (or link to a list of dictionaries and list just the most active)
--
--   support search for dictionaries by name or content
--   provide a simple form to create an empty dictionary
--   links to geneology
--
listOfDictsPage :: WikilonApp
listOfDictsPage w _cap _rq k =
    wikilon_action w listBranches >>= \ lBranchNames -> 
    let title = H.string "Wikilon Dictionaries" in
    k $ Wai.responseLBS HTTP.ok200 [textHtml] $ renderHTML $ do
    H.head $ do
        htmlHeaderCommon w
        H.title title
    H.body $ do
        H.h1 title
        H.b "List of Dictionaries: "
        H.ul $ forM_ lBranchNames $ H.li . hrefDict 
        H.hr 
        formSimpleCreateDict

-- | todo: consider authorization requirements for creating a dictionary
dictCreate :: WikilonApp
dictCreate = app where 
    app = routeOnMethod
        [(HTTP.methodPost, onPost),(HTTP.methodGet, onGet)]
    onGet w _cap _rq k = k $ htmlResponse HTTP.ok200 $ do
        let title = "Create a Dictionary"
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            H.h1 title
            formSimpleCreateDict
    onPost = recvFormPost onFormPost
    onFormPost :: PostParams -> WikilonApp
    onFormPost (ppDictName -> Just d) w _cap _rq k = do
        -- authorize (TODO) then create
        _ <- createDict w d
        -- then goto the named dictionary (be it new or old)
        k $ gotoDict w d
    onFormPost _pp _w _cap _rq k = k $ eBadRequest "invalid dictName"

gotoDict :: Wikilon -> BranchName -> Wai.Response
gotoDict w d = 
    let status = HTTP.seeOther303 in
    let location = (HTTP.hLocation, wikilon_httpRoot w <> uriDict d) in
    let headers = [location, textHtml] in
    let title = H.string $ "Redirect to Dictionary " ++ UTF8.toString (wordToUTF8 d) in
    Wai.responseLBS status headers $ renderHTML $ do
    H.head $ do
        htmlHeaderCommon w
        H.title title
    H.body $ do
        H.h1 title
        H.p $ "If you aren't automatically redirected, goto " <> hrefDict d

-- | Create a dictionary will actually just define the word `id` to `[][]`
-- (the empty identity function) if the dictionary is empty.
createDict :: Wikilon -> BranchName -> IO Bool
createDict w dn = wikilon_action w $ 
    loadBranch dn >>= \ b ->
    branchHead b >>= \ d ->
    let bNull = L.null (Dict.toList d) in
    if not bNull then return False else
    let d' = Dict.unsafeUpdateWord "id" "[][]" d in
    branchUpdate b d' >>
    return True

ppDictName :: PostParams -> Maybe BranchName
ppDictName ps = 
    L.lookup "dictName" ps >>= \ p ->
    let dn = Word $ LBS.toStrict (postParamContent p) in
    guard (isValidWord dn) >>
    return dn

-- | a simple form for creation of a dictionary
formSimpleCreateDict :: HTML
formSimpleCreateDict = H.form ! A.action "d.create" ! A.method "POST" $ do
    let aoWord = A.pattern $ H.stringValue Regex.aoWord 
    H.input ! A.type_ "text" ! A.required "true" ! A.name "dictName" ! aoWord
    H.input ! A.type_ "submit" ! A.value "Create"


