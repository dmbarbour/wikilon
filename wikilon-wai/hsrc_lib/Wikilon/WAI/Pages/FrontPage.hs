{-# LANGUAGE OverloadedStrings #-}
-- | Front page and root URI contents for Wikilon.
module Wikilon.WAI.Pages.FrontPage
    ( wikilonRoot
    , wikilonFrontPage
    , defaultFrontPage
    , frontPageWord

    , hrefAO, hrefABC
    , hrefWikilonGithub
    ) where

import Data.Monoid
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes

wikilonRoot :: WikilonApp
wikilonRoot = app where
    app = justGET onGet where
        -- maybe add OPTIONS eventually?
    onGet = branchOnOutputMedia $
        [(mediaTypeTextHTML, wikilonFrontPage)
        -- maybe add SOAP APIs and others, eventually
        ]

-- | Word to configure the Wikilon front page.
frontPageWord :: Word
frontPageWord = "wikilon:FrontPage"
         
-- | Our front page or primary index for Wikilon. I would like this
-- to be driven by a dictionary, but I haven't quite worked out the
-- detailed designs for this yet.
--
-- Note: I'd prefer to keep most static content out of this Wikilon executable.
-- I probably want to push most content into the dictionary, then develop some
-- initial dictionaries to help developers get started.
--
-- Explanations, tutorials, etc. will be shifted into these extra dictionaries.
--  
wikilonFrontPage :: WikilonApp
wikilonFrontPage w _cap _rq k = k $ 
    Wai.responseLBS HTTP.ok200 [textHtml] $ 
    renderHTML $ defaultFrontPage w

defaultFrontPage :: Wikilon -> HTML
defaultFrontPage w = do
    let title = "Wikilon"
    H.head $ do
        htmlHeaderCommon w
        H.title title
    H.body $ do
        H.h1 title
        explainWikilon w
        H.h2 "Resources"
        wikilonResources w
    -- CONSIDER: providing a one-button click to import a common AO dict


hrefAO, hrefABC, hrefWikilonGithub :: HTML -> HTML
hrefAO = H.a ! A.href (H.unsafeByteStringValue uriAODocs)
hrefABC = H.a ! A.href (H.unsafeByteStringValue uriABCDocs)
hrefWikilonGithub = H.a ! A.href "https://github.com/dmbarbour/wikilon"

explainWikilon :: Wikilon -> HTML
explainWikilon _w = do
    H.p $ "Wikilon is a wiki-inspired software platform and integrated development \n\
        \environment for Awelon project.\n\
        \"
    let lnDicts = H.a ! A.href "d" $ "dictionaries"
    let lnAO = hrefAO "Awelon Object (AO)"
    let lnABC = hrefABC "Awelon Bytecode (ABC)"
    H.p $ "Wikilon contains multiple " <> lnDicts <> ". Each dictionary defines multiple words.\n\
        \Dictionaries correspond roughly to wikis, and words to wiki pages.\n\
        \Unlike conventional wikis, every page in Wikilon defines a concrete mathematical\n\
        \function using the " <> lnAO <> " and  " <> lnABC <> " languages.\n\
        \Functions may be processed to generate static or dynamic content, e.g. (ℝ²→Color) \n\
        \can be sampled to construct raster images, while (µState. Text → (Text*State)) can\n\
        \be compiled into a simple console app or in-browser web app. Rather than normalizing\n\
        \all applications to a common type, Wikilon will enable developers to create ad-hoc new\n\
        \application types and alternative views for existing types.\n\
        \"
    H.p $ "Use of multiple dictionaries enables DVCS-based forking, sharing, stabilization.\n\
        \Open source communities can develop and curate massive dictionaries with millions of\n\
        \words covering everything from high level frameworks to low level details such as\n\
        \SVG fonts and icon packs. AO has no separate notion of libraries or packages, and\n\
        \content that would be static file resources in other languages should be modeled\n\
        \using AO words (accessible for procedural generation and partial evaluation).\n\
        \"
    H.p "As a software platform, Wikilon shall host abstract virtual machines (AVMs) with a simple\n\
        \network model. This enables developers to model stateful services, such as chat servers or\n\
        \multi-user dungeons. Some simple Internet integration is also feasible, via XMLHttpRequest,\n\
        \sockets, or websockets. Further, Wikilon allows override of critical pages, such as this one,\n\
        \by defining words in a configured 'master' dictionary. (Note: any dictionary may be viewed as\n\
        \master via URI /d/dictName/wiki/.) Wikilon can thus be configured into ad-hoc web apps.\n\
        \"
    let lnWikilonGithub = hrefWikilonGithub "github readme and docs"
    H.p $ "See " <> lnWikilonGithub <> " for more details."

wikilonResources :: Wikilon -> HTML
wikilonResources w = listResources where
    listResources = H.ul $ mapM_ H.li $ 
            [rscFrontPage
            ,rscListOfDicts
            ,rscMasterDict
            ,rscMasterFrontPage
            ,rscDbHealth
            ]
    rscFrontPage = (H.a ! A.href "" ! A.rel "self" $ "/") <> " front page"
    rscListOfDicts = (H.a ! A.href "d" $ "/d") <> " list of dictionaries"
    rscDbHealth = (H.a ! A.href "admin/dbHealth" $ "dbHealth") <> " general database stats"
    masterDict = wikilon_master w
    rscMasterDict = hrefDict masterDict <> " - master dictionary"
    rscMasterFrontPage = hrefDictWord masterDict frontPageWord <> " - to configure this page"


