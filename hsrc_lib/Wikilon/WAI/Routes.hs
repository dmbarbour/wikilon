{-# LANGUAGE OverloadedStrings #-}
-- | Primary routes and web-apps for Wikilon.
module Wikilon.WAI.Routes
    ( wikilonRoutes
    , branchURI, branchLink
    , wordURI, wordLink
    ) where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad
import Data.Monoid
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
import Wikilon.Root

-- Primary routes for the Wikilon web service.
wikilonRoutes :: [(BS.ByteString, WikilonApp)]
wikilonRoutes = fmap (first UTF8.fromString) $ 
    [("/d",justGET listOfDicts)
    --,("/d/:d",dictResource)
    -- todo: historical versions
    --,("/d/:d/wordList", justGET dictWordList)
    --,("/d/:d/w/:w", wordResource)
    
--    ,("/u",listOfUsers)
--    ,("/u/:u",singleUser)
    ]

-- Thoughts: I might want to later configure the icons, CSS, and some
-- other content, preferably based on a dictionary. Might make a good
-- 'dictionary app' of sorts.

-- | URI associated with a holistic dictionary
branchURI :: Wikilon -> BranchName -> BS.ByteString
branchURI wiki b = wikilon_httpRoot wiki <> localBranch where
    localBranch = BS.pack (encodePathBytes (branchBytes))
    branchBytes = BS.unpack "/d/" <> BS.unpack b

-- | anchor to branch name
branchLink :: Wikilon -> BranchName -> HTML
branchLink wiki b = 
    let path = HTML.unsafeByteStringValue $ branchURI wiki b in
    let humanText = HTML.unsafeByteString b in
    HTML.a ! Attrib.href path $ humanText

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

listOfDicts :: WikilonApp
-- dictResource, getDict, putDict :: WikilonApp
-- wordResource, getWord, putWord :: WikilonApp

-- | Okay, so our first resource is a collection of dictionaries.
-- There aren't any whole-collection operations at this layer, but
-- some ability to browse the collection.
listOfDicts = branchOnOutputType
    [("text/html", listOfDictsHTML)
    ,("text/plain", listOfDictsText)
    ]

getListOfDicts :: Wikilon -> IO [Branch.BranchName]
getListOfDicts w = Branch.keys <$> readPVarIO (wikilon_dicts w)

-- list the dictionaries by name, one per line.
listOfDictsText :: WikilonApp
listOfDictsText w _cap _rq k = 
    getListOfDicts w >>= \ lNames ->
    k $ Wai.responseLBS HTTP.ok200 [plainText] $ BB.toLazyByteString $ 
        let encName n = BB.byteString n <> BB.charUtf8 '\n' in
        mconcat (encName <$> lNames)

-- list dictionaries using an HTML table, with just a little summary data
listOfDictsHTML :: WikilonApp
listOfDictsHTML w _cap _rq k =
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    k $ Wai.responseLBS HTTP.ok200 [textHtml] $ renderHTML $ do
    HTML.head $ do
        htmlMetaCharsetUtf8
        HTML.title "List of Wikilon Dictionaries"
    HTML.body $ do
        HTML.table $ do
            HTML.tr $ mapM_ HTML.th ["Dictionary", "Versions", "Alive", "E-Tag"]
            forM_ (Branch.toList bset) $ \ (bname,b) -> do
                let d0 = Branch.head b
                HTML.td $ branchLink w bname
                HTML.td $ HTML.toMarkup $ Branch.branchSize b
                HTML.td $ HTML.toMarkup $ not (Dict.null d0)
                HTML.td $ HTML.toMarkup $ toInteger $ Dict.unsafeDictAddr d0
        HTML.p $ HTML.b $ "Count of Dictionaries: " <> HTML.toMarkup (Branch.width bset) 
        HTML.p $ HTML.b $ "Count of Versions: " <> HTML.toMarkup (Branch.volume bset)

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

todo :: String -> WikilonApp
todo msg _ _ _ k = k $ Wai.responseLBS HTTP.status202 [plainText,noCache] $ 
    LazyUTF8.fromString $ "Developer TODO: " ++ msg


getDictWords = branchOnOutputType
    [("text/html", get 
    ,("application/vnd.org.awelon.aodict", getAODictExport)
    ,("text/plain", getPlainWordList)
    ]

getDictWords = todo "words in dict"
putDictWords = todo "PUT words in dict"
-}















