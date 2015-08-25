{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternGuards #-}

module Wikilon.WAI.Pages.DictWord.ClawDef
    ( dictWordClawDef
    , getDictWordClawDef
    , putDictWordClawDef
    , dictWordClawDefEdit
    , formDictWordClawDefEdit
    , parseClawDef
    ) where



import Control.Monad
import Data.Monoid
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai

import Awelon.ABC
import qualified Awelon.ClawCode as Claw

import Wikilon.Time
import Wikilon.Dict.Word
import qualified Wikilon.Dict as Dict

import Wikilon.WAI.ClawUtils
import Wikilon.WAI.Conflicts
import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.WAI.RecvFormPost
-- import qualified Wikilon.WAI.RegexPatterns as Regex

-- | Endpoint that restructs users to Claw format for put and get.
dictWordClawDef :: WikilonApp
dictWordClawDef = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPut, onPut)]
    onGet = branchOnOutputMedia [(mediaTypeClaw, getDictWordClawDef)]
    onPut = branchOnInputMedia [(mediaTypeClaw, putDictWordClawDef)]

textClaw :: HTTP.Header
textClaw = (HTTP.hContentType, HTTP.renderHeader mediaTypeClaw)

-- | Obtain the Claw code for 
getDictWordClawDef :: WikilonApp
getDictWordClawDef = dictWordApp $ \ w dn dw _rq k -> 
    join $ wikilon_action w $ 
    loadDict dn >>= \ d -> return $ 
    let abc = Dict.lookup d dw in
    case parseClawDef abc of
        Just cc -> k $ Wai.responseLBS HTTP.ok200 [textClaw] $ Claw.encode cc
        _ -> k $ Wai.responseLBS HTTP.notFound404 [] $ LBS.empty

putDictWordClawDef :: WikilonApp
putDictWordClawDef = toBeImplementedLater "set word using claw def"

dictWordClawDefEdit :: WikilonApp
dictWordClawDefEdit = app where
    app = routeOnMethod [(HTTP.methodGet, onGet), (HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, getDictEditPage)]
    onPost = branchOnOutputMedia [(mediaTypeTextHTML, 
        recvFormPostP ppClawDefEdit recvClawDefEdit)]

getDictEditPage :: WikilonApp
getDictEditPage = dictWordApp $ \ w dn dw _rq k ->
    join $ wikilon_action w $ 
    loadDictAndTime dn >>= \ (d,tEditOrigin) -> 
    let abc = Dict.lookup d dw in
    let title = "Command Language View and Editor" in
    let status = HTTP.ok200 in
    let headers = [textHtml] in
    return $ k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            H.h1 title
            H.p $ (H.strong "Word:") <> " " <> hrefDictWord dn dw
            formDictWordClawDefEdit dn dw tEditOrigin abc

type ClawDefEditPostParams = (LazyUTF8.ByteString, String)

ppClawDefEdit :: PostParams -> Maybe ClawDefEditPostParams
ppClawDefEdit pp =
    getPostParam "command" pp >>= \ cmd ->
    getPostParam "editOrigin" pp >>= \ eos ->
    return (cmd, LazyUTF8.toString eos)

-- need to refactor this !
recvClawDefEdit :: ClawDefEditPostParams -> WikilonApp
recvClawDefEdit (cmd, sEditOrigin) = dictWordApp $ \ w dn dw _rq k ->
    case Claw.decode cmd of
        Left dcs -> 
            let status = HTTP.badRequest400 in
            let headers = [textHtml, noCache] in
            let title = "Parse Error" in
            k $ Wai.responseLBS status headers $ renderHTML $ do
                H.head $ do
                    htmlMetaNoIndex
                    htmlHeaderCommon w
                    H.title title
                H.body $ do
                    H.h1 title
                    showClawParseError cmd dcs
                    H.h2 "Edit and Resubmit"
                    H.p $ "Do not resubmit without changes."
                    formClawDefEdit' dn dw sEditOrigin cmd
                    H.p $ (H.strong "Word: ") <> hrefDictWord dn dw
        Right cc -> 
            let tEditOrigin = readEditOrigin sEditOrigin in
            let command = Claw.clawToABC cc in
            let abc = mkABC [ABC_Block command, ABC_Block mempty] in
            let lUpdates = [(dw, abc)] in
            join $ wikilon_action w $ 
                loadBranch dn >>= \ b ->
                branchModified b >>= \ tMod ->
                branchHead b >>= \ dHead ->
                branchSnapshot b tEditOrigin >>= \ dOrig ->
                let bsHead = Dict.lookupBytes dHead dw in
                let bsOrig = Dict.lookupBytes dOrig dw in
                let bConflict = (tMod /= tEditOrigin) && (bsHead /= bsOrig) in

                -- Report Edit Conflict
                let onConflict = 
                        let status = HTTP.conflict409 in
                        let headers = [textHtml, noCache] in
                        let title = "Edit Conflict" in
                        return $ k $ Wai.responseLBS status headers $ renderHTML $ do
                            H.head $ do
                                htmlMetaNoIndex
                                htmlHeaderCommon w
                                H.title title
                            H.body $ do
                                H.h1 title
                                H.p $ (H.strong "Word: ") <> hrefDictWord dn dw
                                H.p $ (H.strong "Edit Origin: ") <> H.string sEditOrigin
                                let sHeadVersion = showEditOrigin tMod 
                                H.p $ (H.strong "Head Version: ") <> H.string sHeadVersion

                                H.h2 "Change Report"
                                reportConflicts (LazyUTF8.toString bsOrig) (LazyUTF8.toString bsHead) (show abc)
                                
                                H.h2 "Edit and Resubmit"
                                H.p "At your discretion, you may resubmit without changes."
                                formClawDefEdit' dn dw sHeadVersion cmd
                in

                if bConflict then onConflict else

                case Dict.updateWords lUpdates dHead of
                    Left insErrors -> -- INSERT ERRORS
                        let status = HTTP.conflict409 in
                        let headers = [textHtml, noCache] in
                        let title = "Content Conflicts" in
                        return $ k $ Wai.responseLBS status headers $ renderHTML $ do
                            H.head $ do
                                htmlMetaNoIndex
                                htmlHeaderCommon w
                                H.title title
                            H.body $ do
                                H.h1 title
                                H.p "The proposed update is structurally problematic."
                                H.h2 "Problems"
                                H.ul $ forM_ insErrors $ \ e ->
                                    H.li $ H.string $ show e
                                H.h2 "Edit and Resubmit"
                                H.p "If you can effectively do so from here."
                                formClawDefEdit' dn dw sEditOrigin cmd
                                
                    Right dUpd -> do -- Edit Success!
                        branchUpdate b dUpd 
                        let status = HTTP.seeOther303
                        let dest = (HTTP.hLocation, wikilon_httpRoot w <> uriDictWord dn dw)
                        let headers = [textHtml, noCache, dest] 
                        let title = "Edit Success"
                        return $ k $ Wai.responseLBS status headers $ renderHTML $ do
                            H.head $ do
                                htmlMetaNoIndex
                                htmlHeaderCommon w
                                H.title title
                            H.body $ do
                                H.h1 title
                                H.p $ "If not automatically redirected, return to " <> hrefDictWord dn dw <> "."

formDictWordClawDefEdit :: BranchName -> Word -> T -> ABC -> HTML
formDictWordClawDefEdit d w t (parseClawDef -> Just cc) = do
    formClawDefEdit' d w (showEditOrigin t) (Claw.encode cc)
formDictWordClawDefEdit d w _ _ = do
    formClawDefEdit' d w "(no claw view)" LBS.empty

formClawDefEdit' :: BranchName -> Word -> String -> LazyUTF8.ByteString -> HTML
formClawDefEdit' d w sOrigin sInit = 
    let uriAction = H.unsafeByteStringValue $ uriClawDefEdit d w in
    H.form ! A.method "POST" ! A.action uriAction ! A.id "formClawDefEdit" $ do
        let nLines = H.stringValue $ show $ 1 + LBS.count 10 sInit
        H.textarea ! A.name "command" ! A.lang "claw" ! A.rows nLines ! A.cols "60" $
            H.string $ LazyUTF8.toString sInit -- escapes string for HTML
        H.br
        H.string "Edit Origin: "
        H.input ! A.type_ "text" ! A.name "editOrigin" ! A.value (H.stringValue sOrigin)
        H.string " "
        H.input ! A.type_ "submit" ! A.value "Update as Command"
        let commandLang = href uriClawDocs "claw"
        H.small $ " (cf. " <> commandLang <> ")"

