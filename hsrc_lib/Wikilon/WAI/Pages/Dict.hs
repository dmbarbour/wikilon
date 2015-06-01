{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
-- | Pages for a single dictionary. This might turn into another
-- aggregation module because I want a lot of diverse operations
-- on full dictionaries.
--
-- Note: another format that might be useful for import/export is
-- a tar file, expanding into one file per word?
module Wikilon.WAI.Pages.Dict
    ( dictResource
    , dictWords
    , dictWordsList
    , module Wikilon.WAI.Pages.AODict
    , module Wikilon.WAI.Pages.AODictEdit
    , module Wikilon.WAI.Pages.DictWord
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.Dict.Word
import Wikilon.Dict (Dict)
import qualified Wikilon.Dict as Dict
import Wikilon.Branch (BranchName)
import qualified Wikilon.Branch as Branch
import Wikilon.Root

import qualified Wikilon.Dict.Type as Dict
import qualified Data.VCache.Trie as Trie
import qualified Data.VCache.Trie.Type as Trie
import qualified Data.Array.IArray as A

import Wikilon.WAI.Pages.AODict
import Wikilon.WAI.Pages.AODictEdit
import Wikilon.WAI.Pages.DictWord
        
-- The full 'dictionary resource' will include access to
-- words, histories, issues, subscriptions, etc.. Since
-- there is a lot here, I'll need to use subdirectory URIs
-- for current words and so on.
dictResource :: WikilonApp
dictResource = app where
    app = justGET onGet 
    onGet = branchOnOutputMedia
        [(mediaTypeTextHTML, dictFrontPage)
        ]

dictFrontPage :: WikilonApp
dictFrontPage = dictApp $ \ w dictName rq k -> 
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let b = Branch.lookup' dictName bset in
    --let d = Branch.head b in
    let status = HTTP.status200 in
    let headers = [textHtml] in
    k $ Wai.responseLBS status headers $ renderHTML $ do
    let title = H.unsafeByteString dictName 
    let tmModified = maybe "--" htmlSimpTime $ Branch.modified b
    let origin = Wai.rawPathInfo rq
    H.head $ do
        htmlHeaderCommon w
        H.title title 
    H.body $ do
        H.h1 title
        H.p "This is an AO dictionary front page. I'm still figuring out what should go here."
        -- maybe some content from the dictionary itself
        -- maybe add some banner or CSS from dictionary itself
        -- maybe SVG or icons from dictionary?
        H.h2 "Recent Events"
        H.h2 "Dictionary Health"
        -- Quick Edit?
        -- Quick REPL?
        H.h2 "Resources"
        H.ul $ do
            let browseWords = href (uriDictWords dictName) $ "browse words" 
            let editor = href (uriAODictEdit dictName) $ "AODict editor"
            H.li $ browseWords <> " by name or prefix"
            H.li $ editor <> " view and edit raw fragments of the dictionary"
            H.li $ lnkAODict dictName <> " view full dictionary (low level)"
            
        -- lists of words
            -- plain text lists
            -- lists sorted on prefix
        -- maybe a simple console-like or query application?
        -- after defining apps, probably want to revist them easily
        -- HEALTH information
            -- undefined words & cycles
            -- words that don't compile, compile errors
            -- words that are badly typed
        -- H.h2 "Recent Changes"
        -- H.h2 "Resources"
            -- H.li $ lnkDictWords dictName <> " - dictionary words"
            -- H.li $ lnkMasterDict dictName <> " - view as master"
            -- 
            -- probably want a page to create filtered AODict views
            -- probably want pages for histories and events
            -- 
            -- H.li $ lnkAODict dictName <> " - aodict format "
            -- 

        H.h2 "Browse Dictionary"
        H.p "Small words and common prefixes."
        let lBrowseWords = wordsForBrowsing (Branch.head b) (BS.empty)
        H.ul $ forM_ lBrowseWords $ (H.li . lnkEnt dictName)

        H.hr
        H.div ! A.id "dictFoot" ! A.class_ "footer" $ do
            H.b "Edit:" <> " " <> (formAODictLoadEditor [] dictName ! A.style "display:inline") <> H.br
            H.b "Export:" <> " " <> lnkAODictFile dictName <> " " <> lnkAODictGz dictName <> H.br
            H.b "Import:" <> " " <> formImportAODict origin dictName <> H.br
            H.b "Modified:" <> " " <> tmModified <> H.br

dictWords :: WikilonApp
dictWords = justGET $ branchOnOutputMedia
    [(mediaTypeTextHTML, dictWordsPage)
    ,(mediaTypeTextPlain, dictWordsListText)
    ]

-- | force output media to text\/plain and list words in dictionary
-- one per line. This list of words is suitable for use by a machine
-- rather than use by a human.
dictWordsList :: WikilonApp
dictWordsList = justGET $ branchOnOutputMedia $
    [(mediaTypeTextPlain, dictWordsListText)]

dictWordsListText :: WikilonApp
dictWordsListText = dictApp $ \w dictName rq k ->
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let dict = Branch.head $ Branch.lookup' dictName bset in
    let lWords = Dict.wordsWithPrefix dict (requestedPrefix rq) in
    let encWord (Word wbs) = BB.byteString wbs <> BB.char8 '\n' in
    let content = BB.toLazyByteString $ mconcat $ fmap encWord lWords in
    let status = HTTP.ok200 in
    let etag = eTagN (Dict.unsafeDictAddr dict) in
    let headers = [etag, plainText] in
    k $ Wai.responseLBS status headers content 

-- | Obtain a browseable list of words by name. 
-- 
-- This tries to provide reasonably sized lists of words, based on
-- simple heuristics. 
-- 
-- I would also like to browse words by type, role, domain, etc..
-- But none of that will be supported here. I might need a more
-- sophisticated index to support metadata within the dictionary.
dictWordsPage :: WikilonApp
dictWordsPage = dictApp $ \w dictName rq k ->
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let dict = Branch.head $ Branch.lookup' dictName bset in
    let prefix = requestedPrefix rq in
    let lBrowse = wordsForBrowsing dict prefix in
    let etag = eTagN (Dict.unsafeDictAddr dict) in
    let status = HTTP.ok200 in
    let headers = [textHtml, etag] in
    let title = H.string "Browse Dictionary Words" in
    k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body ! A.class_ "dictWords" $ do
            H.h1 title
            H.strong "Dictionary" <> " " <> hrefDict dictName <> H.br
            unless (BS.null prefix) $
                let htmlPrefix = H.span ! A.class_ "wordPrefix" $ 
                      H.unsafeByteString prefix  
                in
                H.strong "Words Prefix" <> " " <> htmlPrefix <> H.br
            H.h2 "Words"
            H.ul $ forM_ lBrowse (H.li . lnkEnt dictName)

-- | For now, we browse words based on common prefix. I.e. users may
-- select a prefix to expand it.
requestedPrefix :: Wai.Request -> WordPrefix
requestedPrefix = getPrefix . L.lookup "prefix" . Wai.queryString where
    getPrefix (Just (Just p)) = p
    getPrefix _ = BS.empty

type WordPrefix = BS.ByteString

-- | Expand a prefix if there aren't too many words. Otherwise, keep
-- the prefix but extend it into the full prefix. Or, if a prefix has
-- no associated keys, returns an empty list.
expandFini :: Int -> Dict -> WordPrefix -> [Either WordPrefix Word]
expandFini nWidth dict prefix = 
    let tRoot = Trie.lookupPrefix prefix (Dict.dict_defs dict) in
    let lKeys = Trie.keys tRoot in
    let bSmall = (not . lengthAtLeast nWidth) lKeys in
    if bSmall then (Right . Word . (prefix <>)) <$> lKeys else 
    case Trie.trie_root tRoot of
        Nothing -> [] -- no children under this prefix
        Just v -> 
            let tn = deref' v in
            let fullPrefix = prefix <> Trie.trie_prefix tn in
            [Left fullPrefix]

-- | We'll expand the Trie 
wordsForBrowsing :: Dict -> WordPrefix -> [Either WordPrefix Word]
wordsForBrowsing dict prefix = 
    let t0 = Trie.lookupPrefix prefix (Dict.dict_defs dict) in
    case Trie.trie_root t0 of
        Nothing -> [] -- all done
        Just v ->
            let tn = derefc CacheMode0 v in 
            let fullPrefix = prefix <> Trie.trie_prefix tn in
            let bAccept = isJust (Trie.trie_accept tn) in
            let lAccept = if bAccept then [Right (Word fullPrefix)] else [] in
            let lBytes = fmap fst $ L.filter (isJust . snd) $ A.assocs $ Trie.trie_branch tn in
            let expandChild = expandFini 7 dict . (fullPrefix `BS.snoc`) in
            let lChildren = L.concatMap expandChild lBytes in
            lAccept ++ lChildren

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast 0 _ = True
lengthAtLeast n (_:xs) = lengthAtLeast (n-1) xs
lengthAtLeast _ [] = False

lnkEnt :: BranchName -> Either WordPrefix Word -> HTML
lnkEnt dictName = either lnkPrefix (hrefDictWord dictName) where
    lnkPrefix p = 
        let uri = uriDictWordsQPrefix dictName p in
        let msg = H.unsafeByteString p <> "(*)" in
        href uri ! A.class_ "refDictWordPrefix" $ msg

