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
-- Not all of this is implemented yet. To get started fast, I've decided
-- to provide the (stack*(hand*ext)) environment, (unit*(unit*unit)). And
-- no caching is supported at the moment. 
--
module Wikilon.WAI.Pages.REPL
    ( dictRepl
    , formDictClawRepl
    , formDictClawReplS
    ) where

import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Text.Read (readMaybe)
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Network.Wai as Wai
import Database.VCache
import qualified Awelon.ClawCode as CC
import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC
import Awelon.ABC.Eval (Value(..))
import qualified Awelon.ABC.Eval as Eval
import Wikilon.Dict.Word
import Wikilon.Compile (referenceCompile, basicEval)

import Wikilon.WAI.Utils
import Wikilon.WAI.ClawUtils
import Wikilon.WAI.Routes
import Wikilon.WAI.RecvFormPost 
import Wikilon.Store.Root
import qualified Wikilon.Store.Dict as Dict
import qualified Wikilon.Store.Branch as Branch

-- | stateless REPL form for claw code.
formDictClawRepl :: Eval.Quota -> Branch.BranchName -> LazyUTF8.ByteString -> HTML
formDictClawRepl q dn sCommand = 
    let uriAction = H.unsafeByteStringValue (uriRepl dn) in
    H.form ! A.method "GET" ! A.action uriAction ! A.id "formClawRepl" $ do
        let nLines = H.stringValue $ show $ 2 + LBS.count 10 sCommand
        H.textarea ! A.name "command" ! A.lang "claw" ! A.rows nLines ! A.cols "60" $
            H.string $ LazyUTF8.toString sCommand
        H.br
        H.input ! A.type_ "submit" ! A.value "Evaluate"

        H.string " Quota: " 
        let qv = H.stringValue (show (max 0 q))
        H.input ! A.type_ "text" ! A.name "quota" ! A.value qv
                ! A.pattern "[0-9]*" ! A.size "10"

-- | a short form for REPL, accepting only a single line of input.
formDictClawReplS :: Branch.BranchName -> LazyUTF8.ByteString -> HTML
formDictClawReplS dn sCommand =
    let uriAction = H.unsafeByteStringValue (uriRepl dn) in
    H.form ! A.method "GET" ! A.action uriAction ! A.id "formClawRepl" $ do
        let sCmdStr = LazyUTF8.toString sCommand
        let nWidth = max 60 (4 + L.length sCmdStr)
        H.input ! A.type_ "text" ! A.name "command" ! A.lang "claw" 
                ! A.size (H.stringValue (show nWidth))
                ! A.value (H.stringValue sCmdStr)
        H.input ! A.type_ "submit" ! A.value "Evaluate"
        let sQuota = H.stringValue (show defaultQuota)
        H.input ! A.type_ "hidden" ! A.name "quota" ! A.value sQuota

dictRepl :: WikilonApp
dictRepl = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, replPage)]
    onPost = branchOnOutputMedia [(mediaTypeTextHTML, replPost)]
    replPost = toBeImplementedLater "persistent REPL sessions"

queriedReplCommand :: HTTP.Query -> LBS.ByteString
queriedReplCommand q =
    case L.lookup "command" q of
        Just (Just s) -> normalizeNewlines $ LBS.fromStrict s
        _ -> mempty

-- obtain an eval quota from the request query
queriedEvalQuota :: HTTP.Query -> Maybe Eval.Quota 
queriedEvalQuota q = 
    join (L.lookup "quota" q) >>= \ lbs ->
    let s = LazyUTF8.toString $ LBS.fromStrict lbs in
    readMaybe s

defaultQuota :: Eval.Quota
defaultQuota = 10000000

replPage :: WikilonApp
replPage = dictApp $ \ w dn rq k -> 
    let qs = Wai.queryString rq in
    let cmdString = queriedReplCommand qs in
    let quota = maybe defaultQuota id (queriedEvalQuota qs) in
    let footerDict = H.strong "Dictionary:" <> " " <> hrefDict dn in
    let replForm = formDictClawReplS dn cmdString in
    case CC.decode cmdString of
        Left dcs -> -- PARSE ERROR
            let status = HTTP.badRequest400 in
            let headers = [textHtml] in
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
                    replForm
                    H.hr
                    footerDict
        Right cc ->
            readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
            let d = Branch.head $ Branch.lookup' dn bset in
            let abc = CC.clawToABC cc in
            let footerWords = navWords "Words" dn $ L.nub $ Dict.abcWords abc in

            -- for now we'll just do a "get it working soon" compile
            -- (since it's taking forever to support caching, etc.)
            let abcCompiledSimplified = 
                    ABC.abcSimplify $ ABC.abcOps $ 
                    referenceCompile d quota abc 
            in
            let q0 = quota - ABC.abcOpsCount abc in
            let c0 = Eval.Cont abcCompiledSimplified Eval.Return in
            let v0 = Pair Unit (Pair Unit Unit) in
            let evalResult = basicEval v0 c0 q0 in
            
            let status = HTTP.ok200 in
            let headers = [textHtml] in
            let title = case evalResult of
                    Left _ -> "REPL Stuck" 
                    Right _ -> "REPL"
            in
            k $ Wai.responseLBS status headers $ renderHTML $ do
                H.head $ do
                    htmlHeaderCommon w
                    H.title title
                H.body $ do 
                    H.h1 title
                    printEvalResult evalResult
                    replForm
                    H.hr
                    footerWords
                    footerDict

-- TODO: create a REPL variation that does not expand {%word} links
-- within blocks and the continuation, i.e. such that these can still
-- be expressed in terms of human-meaningful words.
--
-- I could use REPL query string to provide lots of different view
-- and evaluation options.

-- heuristically injects vertical space into printed bytecode
printABC :: [ABC.Op] -> HTML
printABC = wrap . p [] 0 where
    wrap = (H.pre ! A.lang "abc") . H.code
    w = 60 -- configured width
    p s n ops | (n >= w) = "\n" <> p s 0 ops
    p s n (ABC.ABC_Block abc : ops) =
        "[" <> p (ops:s) (n+1) (ABC.abcOps abc)
    p (cc:s) n [] = "]" <> p s (n+1) cc
    p s n (op@(ABC.ABC_Text _) : ops) = do
        when (n > 0) (H.string "\n")
        H.string (show op)
        p s 1 ops
    p s n (op:ops) =
        let str = show op in
        let len = L.length str in
        let bNL = (w < (len + n)) in
        if bNL then "\n" <> H.string str <> p s len ops
               else H.string str <> p s (len + n) ops
    p [] _ [] = mempty

printCont :: Eval.Cont -> HTML
printCont cc = printABC (ABC.quote cc) ! A.class_ "continuation"

printVal :: Eval.Value -> HTML
printVal v = printABC (ABC.quote v) ! A.class_ "value"

-- full continuation including stuck_curr
stuckFullCont :: Eval.Stuck -> Eval.Cont
stuckFullCont s =
    let cc = Eval.stuck_cont s in
    let k = Eval.cc_cont cc in
    let k' = (Eval.stuck_curr s : k) in
    cc { Eval.cc_cont = k' }

printStuck :: Eval.Stuck -> HTML
printStuck s = do
    H.strong "Quoted Value:"
    printVal (Eval.stuck_arg s)
    H.br
    H.strong "Continuation:"
    printCont (stuckFullCont s)

printEvalResult :: Either Eval.Stuck Eval.Value -> HTML
printEvalResult = either printStuck printVal
            

-- THOUGHTS: I might also want:
--  a resource to extract the entire session
--  a resource to extract the result as ABC (quoted)
--
-- But I think these might need different URIs.
--