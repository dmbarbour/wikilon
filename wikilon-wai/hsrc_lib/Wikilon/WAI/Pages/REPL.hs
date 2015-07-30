{-# LANGUAGE OverloadedStrings #-}

-- | Wikilon's REPL (Read Eval Print Loop).
--
-- The plan is to model two variations on the REPL:
--
-- (a) stateless, GET-based, bookmarkable, view oriented
-- (b) stateful, POST-based, sessions, functional forums
--
-- These might be integrated, e.g. by recognizing when a stateless REPL
-- command corresponds to a viable post (e.g. if the first word in the
-- command is a thread identifer (otherwise offering a new thread). The
-- stateful sessions might be modeled as a branching thread or notebook,
-- as a dictionary application (threads and posts as words in dictionary)
-- 
-- Wikilon's REPL will be purely functional, which greatly simplifies
-- the idea of branching and exploring different futures. It will operate
-- on a *void* environment, i.e. one initially devoid of constraints, and
-- hence the entire session can be understood as a function (even though
-- we'll generally render only a fragment of the environment). 
--
-- For *performance* it will be necessary to cache our computations.
-- This might be triggered by a REPL command whose first entry is a 
-- word. That is, we obtain a cached value or type corresponding to
-- the first word, then extend from there. As needed, we may recompute
-- caches, and may favor caching only after a given quota has been
-- reached.
--
module Wikilon.WAI.Pages.REPL
    ( dictRepl
    , formDictClawRepl
    ) where

import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Network.Wai as Wai
import Database.VCache
import qualified Awelon.ClawCode as CC

import Wikilon.WAI.Utils
import Wikilon.WAI.ClawUtils
import Wikilon.WAI.Routes
import qualified Wikilon.Store.Dict as Dict
import qualified Wikilon.Store.Branch as Branch

-- | stateless REPL form for claw code.
formDictClawRepl :: Branch.BranchName -> LazyUTF8.ByteString -> HTML
formDictClawRepl dn sCommand = 
    let uriAction = H.unsafeByteStringValue (uriRepl dn) in
    H.form ! A.method "GET" ! A.action uriAction ! A.id "formClawRepl" $ do
        let nLines = H.stringValue $ show $ 1 + LBS.count 10 sCommand
        H.textarea ! A.name "command" ! A.lang "claw" ! A.rows nLines ! A.cols "60" $
            H.string $ LazyUTF8.toString sCommand
        H.br
        H.input ! A.type_ "submit" ! A.value "Evaluate"

dictRepl :: WikilonApp
dictRepl = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, replPage)]
    onPost = branchOnOutputMedia [(mediaTypeTextHTML, replPost)]

queriedReplCommand :: HTTP.Query -> LBS.ByteString
queriedReplCommand q =
    case L.lookup "command" q of
        Just (Just s) -> LBS.fromStrict s
        _ -> mempty

replPage :: WikilonApp
replPage = dictApp $ \ w dn rq k -> 
    let cmdString = queriedReplCommand (Wai.queryString rq) in
    let footerDict = H.strong "Dictionary:" <> " " <> hrefDict dn in
    case CC.decode cmdString of
        Left dcs -> -- PARSE ERROR
            let status = HTTP.badRequest400 in
            let headers = [textHtml, noCache] in
            let title = "REPL Parse Error" in
            k $ Wai.responseLBS status headers $ renderHTML $ do
                H.head $ do
                    htmlMetaNoIndex
                    htmlHeaderCommon w
                    H.title title
                H.body $ do
                    H.h1 title
                    showClawParseError cmdString dcs
                    H.h2 "Edit and Resubmit"
                    formDictClawRepl dn cmdString
                    H.hr
                    footerDict
        Right cc ->
            let abc = CC.clawToABC cc in
            let footerWords = navWords "Words" dn $ L.nub $ Dict.abcWords abc in
            evalCC w dn cc >>= \ result ->
            let status = HTTP.ok200 in
            let headers = [textHtml] in
            let title = "REPL" in
            k $ Wai.responseLBS status headers $ renderHTML $ do
                H.head $ do
                    htmlHeaderCommon w
                    H.title title
                H.body $ do
                    H.h1 title
                    showEvalResult dn result
                    formDictClawRepl dn cmdString
                    -- next: option to submit as persistent...
                        -- (or switch to a persistent-mode REPL)
                    H.hr
                    footerWords
                    footerDict

type EvalResult = HTML -- for now

-- 
evalCC :: Wikilon -> Branch.BranchName -> CC.ClawCode -> IO EvalResult
evalCC _ _ _ = return $ H.string "TODO: EVAL RESULT"

showEvalResult :: Branch.BranchName -> EvalResult -> HTML
showEvalResult = const id


replPost :: WikilonApp
replPost = toBeImplementedLater "clawRepl: persistent sessions via dictionary"

-- THOUGHTS: I might also want:
--  a resource to extract the entire session
--  a resource to extract the result as ABC (quoted)
--
-- But I think these might need different URIs.
--
