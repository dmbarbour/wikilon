{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternGuards #-}
-- | Primary routes and web-apps for Wikilon.
module Wikilon.WAI.Routes
    ( wikilonRoutes

    , dictURI, dictLink, dictCap
    , wordURI, wordLink, wordCap
    ) where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attrib
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.URL
import Wikilon.Branch (BranchName)
import qualified Wikilon.Branch as Branch
import Wikilon.Dict (Word(..))
import qualified Wikilon.Dict as Dict
import qualified Wikilon.Dict.AODict as AODict
import Wikilon.Root

-- Primary routes for the Wikilon web service.
wikilonRoutes :: [(BS.ByteString, WikilonApp)]
wikilonRoutes = fmap (first UTF8.fromString) $ 
    [("/", wikilonRoot)
    ,("/d", listOfDicts)
    ,("/d/:d", dictResource)
    ,("/d/:d/w", dictWords)
    --,("/d/:d/w/:w", dictWord)
    --,("/d/:d/w/:w/name", 
    --,("/d/:d/w/:w/deps", dictWordDeps)
    --,("/d/:d/w/:w/clients", dictWordClients)
    --,("/d/:d/w*name", dictWordList)
    --,("/d/:d/w*deps", dictWordListDeps)
    --,("/d/:d/w*clients", dictWordListClients)
    -- todo: historical versions
    
--    ,("/u",listOfUsers)
--    ,("/u/:u",singleUser)
    ]

-- Thoughts: I might want to later configure the icons, CSS, and some
-- other content, preferably based on a dictionary. Might make a good
-- 'dictionary app' of sorts.

-- | URI associated with a holistic dictionary
dictURI :: Wikilon -> BranchName -> BS.ByteString
dictURI wiki b = wikilon_httpRoot wiki <> localBranch where
    localBranch = BS.pack (encodePathBytes (branchBytes))
    branchBytes = BS.unpack "/d/" <> BS.unpack b

-- | anchor to branch name
dictLink :: Wikilon -> BranchName -> HTML
dictLink wiki b = 
    let path = HTML.unsafeByteStringValue $ dictURI wiki b in
    let humanText = HTML.unsafeByteString b in
    HTML.a ! Attrib.href path $ humanText

dictCap :: Captures -> Maybe BranchName
dictCap = L.lookup "d"

-- | URI associated for a specific word in a named dictionary
wordURI :: Wikilon -> BranchName -> Word -> BS.ByteString
wordURI wiki b (Word w) = wikilon_httpRoot wiki <> localWord where
    localWord = BS.pack (encodePathBytes (branchBytes <> wordBytes))
    branchBytes = BS.unpack "/d/" <> BS.unpack b
    wordBytes = BS.unpack "/w/" <> BS.unpack w

wordLink :: Wikilon -> BranchName -> Word -> HTML
wordLink wiki b w@(Word wbs) = 
    let path = HTML.unsafeByteStringValue $ wordURI wiki b w in
    let humanText = HTML.unsafeByteString wbs in
    HTML.a ! Attrib.href path $ humanText

wordCap :: Captures -> Maybe (BranchName, Word)
wordCap cap =
    L.lookup "d" cap >>= \ d ->
    L.lookup "w" cap >>= \ w ->
    return (d, Word w)

wikilonRoot, wikilonFrontPage :: WikilonApp
wikilonRoot = justGET $ branchOnOutputMedia $
    [("text/html", wikilonFrontPage)]
    -- SOAP API?
    -- JSON Overview?
    -- richer OPTIONS?

-- | Our front page or primary index for Wikilon. I would like this
-- to be driven by a dictionary, but I haven't quite worked out the
-- detailed design for that yet.
wikilonFrontPage _w _cap _rq k = k response where
    response = Wai.responseLBS HTTP.ok200 [textHtml] body
    body = renderHTML $ do
        HTML.head $ do
            htmlMetaCharsetUtf8
            HTML.title "Wikilon"
        HTML.body $ do
            HTML.h1 "Wikilon"
            sectionWikilon
            HTML.h2 "Resources"
            sectionResources

    sectionWikilon = do
        HTML.p "This is a temporary front page for Wikilon."
        HTML.p "Eventually, this page should become configurable through\n\
               \editing of a dictionary. Which dictionary, and which words,\n\
               \are not yet decided. Details will be added later."
    sectionResources = do
        HTML.p "Wikilon tries to provide a 'hackable' URI for users, though\n\
               \programmatic link construction is discouraged. RESTful design\n\
               \is a long term goal. Getting started:"
        HTML.ul $ mapM_ HTML.li $ rscList
    rscList =
        [rscFrontPage
        ,rscListOfDicts
        ]
    rscFrontPage = do
        HTML.a ! Attrib.href "/" ! Attrib.rel "self" $ "/"
        " front page, this page"
    rscListOfDicts = do
        HTML.a ! Attrib.href "/d" $ "/d"
        " list of dictionaries"

-- | Okay, so our first resource is a collection of dictionaries.
-- There aren't any whole-collection operations at this layer, but
-- some ability to browse the collection.
listOfDicts :: WikilonApp
listOfDicts = justGET $ branchOnOutputMedia
    [("text/html", listOfDictsHTML)
    ,("text/plain", listOfDictsText)
    ]

-- list the dictionaries by name, one per line.
listOfDictsText :: WikilonApp
listOfDictsText w _cap _rq k = 
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let lNames = Branch.keys bset in
    let etag = eTagN (Branch.unsafeBranchSetAddr bset) in
    k $ Wai.responseLBS HTTP.ok200 [plainText,etag] $ BB.toLazyByteString $ 
        let encName n = BB.byteString n <> BB.charUtf8 '\n' in
        mconcat (encName <$> lNames)

-- list dictionaries using an HTML table, with just a little summary data
-- TODO: add a form for adding a new dictionary. Maybe use `/d(new)` form?
listOfDictsHTML :: WikilonApp
listOfDictsHTML w _cap _rq k =
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let etag = eTagNW (Branch.unsafeBranchSetAddr bset) in
    k $ Wai.responseLBS HTTP.ok200 [textHtml, etag] $ renderHTML $ do
    HTML.head $ do
        htmlMetaCharsetUtf8
        HTML.title "List of Wikilon Dictionaries"
    HTML.body $ do
        HTML.table $ do
            HTML.tr $ mapM_ HTML.th ["Dictionary", "Versions", "Head-ETag"]
            forM_ (Branch.toList bset) $ \ (bname,b) -> do
                let d0 = Branch.head b
                HTML.td $ dictLink w bname
                HTML.td $ HTML.toMarkup $ Branch.branchSize b
                HTML.td $ HTML.toMarkup $ toInteger $ Dict.unsafeDictAddr d0
        HTML.p $ HTML.b $ "Count of Dictionaries: " <> HTML.toMarkup (Branch.width bset) 
        HTML.p $ HTML.b $ "Count of Versions: " <> HTML.toMarkup (Branch.volume bset)


-- The full 'dictionary resource' will include access to
-- words, histories, issues, subscriptions, etc.. Since
-- there is a lot here, I'll need to use subdirectory URIs
-- for current words and so on.
dictResource :: WikilonApp
dictResource = justGET dictFrontPage

dictFrontPage :: WikilonApp
dictFrontPage = todo "Dictionary"

todo :: String -> WikilonApp
todo msg _ _ _ k = k $ Wai.responseLBS HTTP.status202 [textHtml,noCache] $ 
    renderHTML $ do 
    let msgMarkup = HTML.toMarkup msg
    HTML.head $ HTML.title msgMarkup
    HTML.body $ HTML.p $ (HTML.b "TODO: " <> msgMarkup)

dictWords :: WikilonApp
dictWords = routeOnMethod 
    [(HTTP.methodGet, getDictWords)
    --,(HTTP.methodPut, putDictWords)
    ]

getDictWords :: WikilonApp
getDictWords = branchOnOutputMedia
    [("text/vnd.org.awelon.aodict", exportDictWords)
    ] 

-- | our primary export model for dictionaries. 
--
-- TODO: I may need authorization for some dictionaries.
exportDictWords :: WikilonApp
exportDictWords w (dictCap -> Just dictName) _rq k = do
    branchSet <- readPVarIO (wikilon_dicts w)
    let d = Branch.head $ Branch.lookup' dictName branchSet
    let etag = eTagN $ Dict.unsafeDictAddr d
    let aodict = (HTTP.hContentType, "text/vnd.org.awelon.aodict")
    let status = HTTP.ok200
    let headers = [aodict,etag]
    k $ Wai.responseLBS status headers $ AODict.encode d
exportDictWords _ _ _ k = -- should be impossible...
    k $ genericServerFailure "no dictionary identified" 



{-        
singleDict = routeOnMethod 
    [(HTTP.methodGet, getDict)
    ]
dictWordsList = routeOnMethod [(HTTP.methodGet, getDictWordList)]
singleDictWord = routeOnMethod
    [(HTTP.methodGet, getDictWord)
    ,(HTTP.methodPut, putDictWord)
    -- patch?
    ]
listOfUsers = routeOnMethod
    [(HTTP.methodGet, getListOfUsers)
    -- patch?
    ]
singleUser = routeOnMethod
    [(HTTP.methodGet, getUser)
    ]

-- Now I probably need to `routeOnAccept`. Either that, or I need to
-- find some way to convert types appropriately. But routeOnAccept is
-- probably the better option here.

getFrontPage = todo "front page"
getListOfDicts = todo "list of dicts"
getDict = todo "dict page"
getDictWord = todo "word in dict"
putDictWord = todo "PUT word in dict"
getListOfUsers = todo "list of users"
getUser = todo "get user"

getDictWords = branchOnOutputMedia
    [("text/html", get 
    ,("application/vnd.org.awelon.aodict", getAODictExport)
    ,("text/plain", getPlainWordList)
    ]

getDictWords = todo "words in dict"
putDictWords = todo "PUT words in dict"
-}















