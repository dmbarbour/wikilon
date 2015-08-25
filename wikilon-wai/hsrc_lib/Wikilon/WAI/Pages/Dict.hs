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
import Data.Either (isLeft)
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import qualified Wikilon.Dict as Dict

import Wikilon.WAI.Pages.AODict
import Wikilon.WAI.Pages.AODictEdit
import Wikilon.WAI.Pages.DictWord
import Wikilon.WAI.Pages.REPL
        
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
    wikilon_action w (loadDictAndTime dictName) >>= \ (d,tMod) ->
    let status = HTTP.status200 in
    let headers = [textHtml, eTagTW tMod] in
    k $ Wai.responseLBS status headers $ renderHTML $ do
        let title = H.string "Dictionary: " <> H.unsafeByteString (wordToUTF8 dictName) 
        let origin = Wai.rawPathInfo rq
        H.head $ do
            htmlHeaderCommon w
            H.title title 
        H.body $ do
            H.h1 title
            formDictClawReplS defaultQuota dictName mempty

            let lnAwelonObject = href uriAODocs $ "Awelon Object (AO)" 
            H.p $ "This is an " <> lnAwelonObject <> " dictionary."
            H.p $ "To add here: dictionary edit and curation policy, security info,\n\
                  \external bindings or users (subscriptions, etc.)"
            -- maybe some content from the dictionary itself
            -- maybe add some banner or CSS from dictionary itself
            -- maybe SVG or icons from dictionary?
            H.h2 "Recent Events"
            H.p $ "What might go here: recent updates, active background tasks,\n\
                  \active bound AVMs, recent users or REPLs, ongoing compilation."
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
            let lBrowseWords = wordsForBrowsing 24 48 d BS.empty
            H.ul $ forM_ lBrowseWords $ (H.li . lnkEnt dictName)
            H.p $ "Long term, I'd like to support browsing by type, domain, role, definition structure, etc."

            H.hr
            H.div ! A.id "dictFoot" ! A.class_ "footer" $ do
                H.b "Export:" <> " " <> lnkAODictFile dictName <> " " <> lnkAODictGz dictName <> H.br
                H.b "Import:" <> " " <> formImportAODict origin dictName <> H.br
                H.b "Modified:" <> " " <> htmlSimpTime tMod <> H.br
                H.b "Edit AODict:" <> " " <> (formAODictLoadEditor [] dictName ! A.style "display:inline") <> H.br

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
    wikilon_action w (loadDictAndTime dictName) >>= \ (dict,tMod) ->
    let lWords = Dict.wordsWithPrefix dict (requestedPrefix rq) in
    let encWord (Word wbs) = BB.byteString wbs <> BB.char8 '\n' in
    let content = BB.toLazyByteString $ mconcat $ fmap encWord lWords in
    let status = HTTP.ok200 in
    let etag = eTagTW tMod in
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
    wikilon_action w (loadDictAndTime dictName) >>= \ (dict,tMod) ->
    let prefix = requestedPrefix rq in
    let lBrowse = wordsForBrowsing 24 48 dict prefix in
    let etag = eTagTW tMod in
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
requestedPrefix :: Wai.Request -> Dict.WordPrefix
requestedPrefix = getPrefix . L.lookup "prefix" . Wai.queryString where
    getPrefix (Just (Just s)) = s
    getPrefix _ = BS.empty


type Ent = Either Dict.WordPrefix Word

-- | Expose a menu of words for users to browse based on a common prefix.
-- (I would eventually like to support browsing words on other properties.)
wordsForBrowsing :: (Dict.DictSplitPrefix dict) 
    => Int -> Int -> dict -> Dict.WordPrefix -> [Ent]
wordsForBrowsing nTargetWidth nMaxWidth dict prefix = 
    let expandPrefix = Dict.splitOnPrefixWords dict in
    let expandEnt = either expandPrefix (return . Right) in
    -- fill a menu of prefix options.
    let loopToFillMenu n l =
            let bBigEnough = (n >= nTargetWidth) in
            let bCannotExpand = L.null (L.filter isLeft l) in
            let bDone = bBigEnough || bCannotExpand in
            if bDone then l else
            let l' = L.concatMap expandEnt l in
            let n' = L.length l' in
            if (n' > nMaxWidth) then l else -- expands too far
            loopToFillMenu n' l'
    in
    -- expand prefixes that lead to exactly one element
    let finishPrefix p = case expandPrefix p of
            [x] -> x -- just one word or prefix
            _ -> Left p -- more than one word
    in
    let finishEnt = either finishPrefix Right in
    let l0 = expandPrefix prefix in
    finishEnt <$> loopToFillMenu (L.length l0) l0

lnkEnt :: BranchName -> Ent -> HTML
lnkEnt dictName = either lnkPrefix lnkWord where
    lnkWord = hrefDictWord dictName
    lnkPrefix p = 
        let uri = uriDictWordsQPrefix dictName p in
        let msg = H.unsafeByteString p <> "..." in
        href uri ! A.class_ "refDictWordPrefix" $ msg

-- TODO: delete a list of words


