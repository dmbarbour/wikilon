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
import qualified Data.ByteString.UTF8 as UTF8
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

import qualified Data.VCache.Trie as Trie
import qualified Data.VCache.Trie.Type as Trie
import qualified Data.Array.IArray as A

import Wikilon.WAI.Pages.AODict
import Wikilon.WAI.Pages.AODictEdit
import Wikilon.WAI.Pages.DictWord

import Wikilon.Store.Dict (Dict)
import qualified Wikilon.Store.Dict as Dict
import qualified Wikilon.Store.Dict.Type as Dict
import Wikilon.Store.Branch (BranchName)
import qualified Wikilon.Store.Branch as Branch
import Wikilon.Store.Root

        
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
    readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
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
        let lnAwelonObject = href uriAODocs $ "Awelon Object (AO)" 
        H.p $ "This is an " <> lnAwelonObject <> " dictionary."
        H.p $ "To add here: dictionary edit and curation policy, security info,\n\
              \external bindings or users (subscriptions, etc.)"
        -- maybe some content from the dictionary itself
        -- maybe add some banner or CSS from dictionary itself
        -- maybe SVG or icons from dictionary?
        H.h2 "Recent Events"
        H.p $ "What might go here: recent updates, active background tasks,\n\
              \active bound AVMs, recent users or REPLs."
        H.h2 "Dictionary Health"
        -- Quick Edit?
        -- Quick REPL?
        H.p $ "What might go here: compilation failures, typecheck failures,\n\
              \linter failures, unproven assertions (from annotations),  TODO\n\
              \annotations (perhaps with expiration dates), etc.."
        H.h2 "Resources"
        H.ul $ do
            let browseWords = href (uriDictWords dictName) $ "browse words" 
            let editor = href (uriAODictEdit dictName) $ "AODict editor"
            H.li $ browseWords <> " by name or prefix"
            H.li $ editor <> " view and edit raw fragments of the dictionary"
            H.li $ lnkAODict dictName <> " view full dictionary (low level)"
            
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

        H.h2 "Browse Words by Name"
        let lBrowseWords = wordsForBrowsing 24 40 (Branch.head b) (BS.empty)
        H.ul $ forM_ lBrowseWords $ (H.li . lnkEnt dictName)
        H.p $ "Long term, I'd like to support browsing by type, domain, role, definition structure, etc."

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
    readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
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
    readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
    let dict = Branch.head $ Branch.lookup' dictName bset in
    let prefix = requestedPrefix rq in
    let lBrowse = wordsForBrowsing 24 40 dict prefix in
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
requestedPrefix :: Wai.Request -> Prefix
requestedPrefix = getPrefix . L.lookup "prefix" . Wai.queryString where
    getPrefix (Just (Just s)) = s
    getPrefix _ = BS.empty

-- developing a simple utility for browsing a trie
type Prefix = UTF8.ByteString -- includes trie_prefix from node!
type Key = UTF8.ByteString
type Entry a = Either (Prefix, Trie.Node a) Key

-- expand a list of entries at least one UTF8 character. We'll assume
-- a good UTF8 string if its last octet is less than 0x80. The goal
-- here is that I shouldn't need to worry about whether a prefix is a
-- valid UTF8 string.
expandEntUTF8 :: Entry a -> [Entry a]
expandEntUTF8 x@(Right _) = [x]
expandEntUTF8 (Left (p,tn)) = expandEntUTF8' p tn

expandEntUTF8' :: Prefix -> Trie.Node a -> [Entry a]
expandEntUTF8' prefix tnParent =
    let bAccept = isJust (Trie.trie_accept tnParent) in
    let lAccept = if bAccept then [Right prefix] else [] in
    let toChildEnts (byte,getChildRef) = 
            maybeToList getChildRef >>= \ v ->
            let tnChild = derefc CacheMode0 v in
            let childPrefix = mconcat [prefix, BS.singleton byte, Trie.trie_prefix tnChild] in
            let bOkUTF8 = BS.last childPrefix < 0x80 in
            if bOkUTF8 then [Left (childPrefix, tnChild)] else 
            -- prefix in middle of UTF8 char!
            expandEntUTF8' childPrefix tnChild 
    in
    let lChildren = L.concatMap toChildEnts $ A.assocs $ Trie.trie_branch tnParent in
    lAccept ++ lChildren

-- | We'll expand the Trie until it surpasses a target width, unless doing
-- so causes it to surpass the maximum width. Expansions are uniformly
-- breadth-first. At least one expansion is performed regardless.
wordsForBrowsing :: Int -> Int -> Dict -> Prefix -> [Either Prefix Word]
wordsForBrowsing nTargetWidth nMaxWidth dict prefix = 
    let t0 = Trie.lookupPrefix prefix (Dict.dict_defs dict) in
    case Trie.trie_root t0 of
        Nothing -> [] -- prefix has no content
        Just v ->
            let tn = derefc CacheMode0 v in 
            let fullPrefix = prefix <> Trie.trie_prefix tn in
            let loopToFillMenu n l =
                    if (n >= nTargetWidth) then l else
                    let l' = L.concatMap expandEntUTF8 l in
                    let n' = L.length l' in
                    if (n' == n) then l' else  -- could not expand
                    if (n' > nMaxWidth) then l else -- expands too far
                    loopToFillMenu n' l'
            in
            let l0 = expandEntUTF8' fullPrefix tn in
            let n0 = L.length l0 in
            finishEnt <$> loopToFillMenu n0 l0


isLeafNode :: Trie.Node a -> Bool
isLeafNode = L.null . L.filter isJust . A.elems . Trie.trie_branch

-- if a final child has no children, we'll translate it to a key
finishEnt :: Entry a -> Either Prefix Word
finishEnt (Right x) = Right (Word x)
finishEnt (Left (p,tn)) | isLeafNode tn = Right (Word p)
                        | otherwise = Left p

lnkEnt :: BranchName -> Either Prefix Word -> HTML
lnkEnt dictName = either lnkPrefix (hrefDictWord dictName) where
    lnkPrefix p = 
        let uri = uriDictWordsQPrefix dictName p in
        let msg = H.unsafeByteString p <> "(*)" in
        href uri ! A.class_ "refDictWordPrefix" $ msg

-- TODO: delete a list of words





