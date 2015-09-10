{-# LANGUAGE OverloadedStrings #-}
-- | Front page and root URI contents for Wikilon.
module Wikilon.WAI.Pages.FrontPage
    ( wikilonRoot
    , wikilonFrontPage
    , defaultFrontPage

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
{-
-- | Word to configure the Wikilon front page.
frontPageWord, defaultCSSWord :: Word
frontPageWord = "wikilon.frontPage"
defaultCSSWord = "wikilon.css"
-}

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
        explainWikilon
        H.h2 "Resources"
        wikilonResources
    -- CONSIDER: providing a one-button click to import a common AO dict

explainWikilon :: HTML
explainWikilon = do
    H.p $ "Wikilon is a wiki-inspired development environment and software platform \n\
        \for Awelon project. A codebase is modeled by an " <> href uriAODocs "AO dictionary" <> ",\n\
        \of which Wikilon " <> href "d" "may host many" <> ". Support for multiple dictionaries\n\
        \is primarily for development purposes, e.g. forking, branching, merging, preserving\n\
        \specific versions. Most dictionaries will share most words and definitions.\n"
    H.p $ "Each word in a dictionary defines a pure function. Many functions will simply\n\
        \provide static content - e.g. texts, images, web pages. A goal of Awelon project\n\
        \is that static content should be provided within a programming language such that\n\
        \it is easily subject to abstraction, procedural generation, partial evaluation,\n\
        \typechecking, unit testing, and other conveniences of programming.\n"
    H.p $ "As a software platform, Wikilon will leverage simple name and type conventions\n\
        \to provide web resources from a dictionary. Forums, wikis, spreadsheets, and more\n\
        \can be directly hosted from a dictionary - assuming we're clever about it and make\n\
        \effective use of a cache. The front page, stylesheet, favicon, and more will be\n\
        \configurable through editing of dictionary. Future versions of Wikilon might host\n\
        \abstract virtual machines to model conventional web services.\n"

wikilonResources ::  HTML
wikilonResources = listResources where
    listResources = H.ul $ mapM_ H.li $ 
            [rscFrontPage
            ,rscListOfDicts
            ]
    rscFrontPage = (H.a ! A.href "" ! A.rel "self" $ "/") <> " front page"
    rscListOfDicts = (H.a ! A.href "d" $ "/d") <> " list of dictionaries"


