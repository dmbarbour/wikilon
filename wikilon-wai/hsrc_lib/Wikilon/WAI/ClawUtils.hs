{-# LANGUAGE OverloadedStrings #-} 

module Wikilon.WAI.ClawUtils 
    ( parseClawDef
    , showClawParseError
    ) where

import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Awelon.ABC
import qualified Awelon.ClawCode as Claw
import Wikilon.WAI.Utils
import Wikilon.WAI.Routes

-- | A 'Claw' definition must have form `[command][]`.
--
-- That is, our definition is a plain old block of code, and our
-- compiler is the identity function []. This will always be the
-- case for words defined via the claw definition interfaces.
--
-- Commands should always include their own namespace (if it is
-- other than the default).
parseClawDef :: ABC -> Maybe Claw.ClawCode
parseClawDef abc =
    let topLevel = L.filter (not . abcSP) $ abcOps abc in
    case topLevel of
    [ABC_Block command, ABC_Block compiler] | ok -> return cc where
        ok = L.null $ abcOps compiler
        cc = Claw.clawFromABC command
    _ -> Nothing

abcSP :: Op -> Bool
abcSP (ABC_Prim op) = (ABC_SP == op) || (ABC_LF == op)
abcSP _ = False

-- simplistic error analysis and report for humans
showClawParseError :: LBS.ByteString -> Claw.DecoderState -> HTML
showClawParseError s dcs = do
    let commandLang = href uriClawDocs "command language for awelon (claw)" 
    H.p $ "Expecting content in the " <> commandLang <> "."
    let badText = Claw.dcs_text dcs
    if LBS.null badText 
       then H.p $ "A block was not closed. Add the missing ']' character(s)."
       else let lenOK = (LBS.length s - LBS.length (Claw.dcs_text dcs)) in
            let okText = LBS.take lenOK s in
            H.pre ! A.lang "claw" $ H.code $ do
                H.span ! A.class_ "parseValid" $ H.string (LazyUTF8.toString okText)
                H.span ! A.class_ "parseError" $ H.string (LazyUTF8.toString badText) 


