{-# LANGUAGE OverloadedStrings #-}
-- | TODO: 
module Wikilon.WAI.Pages.Admin
    ( dbHealth
    ) where

import Data.Monoid

import qualified Text.Blaze.Html5 as H
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

import Wikilon.WAI.Utils


dbHealth :: WikilonApp
dbHealth = justGET $ branchOnOutputMedia $
    [(mediaTypeTextHTML, dbHealthPage)]

-- note: since I don't have access to database statistics directly at this
-- time, I'll need to provide 
dbHealthPage :: WikilonApp
dbHealthPage w _cap _rq k =
    let title = "Database Health" in
    let status = HTTP.ok200 in
    let headers = [textHtml] in
    k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            H.h1 title
            H.strong "TODO" <> " provide database and health statistics, maybe error reports"

