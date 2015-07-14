{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.Conflicts
    ( reportConflicts
    ) where

import Control.Monad
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.Diff3 as Diff3
import Wikilon.WAI.Utils

twoWayMerge :: String -> String -> HTML
twoWayMerge sHead sEdit = 
    let lChunks = Diff.getGroupedDiff sHead sEdit in
    forM_ lChunks $ \ chunk -> case chunk of
        Diff.First s -> styleHead $ H.string s
        Diff.Second s -> styleEdit $ H.string s
        Diff.Both s _ -> styleOrig $ H.string s

threeWayMerge :: String -> String -> String -> HTML
threeWayMerge sHead sOrig sEdit =
    let lChunks = Diff3.diff3 sHead sOrig sEdit in
    forM_ lChunks $ \ chunk -> case chunk of
        Diff3.LeftChange s -> styleHead $ H.string s
        Diff3.RightChange s -> styleEdit $ H.string s
        Diff3.Unchanged s -> styleOrig $ H.string s 
        Diff3.Conflict h o e -> styleConflict $ do
            barrierConflict "("
            styleHead $ H.string h
            barrierConflict "|"
            styleOrig $ H.string o
            barrierConflict "|"
            styleEdit $ H.string e
            barrierConflict ")"

merge2Box, merge3Box :: HTML -> HTML
merge2Box = codeBox "diffMerge2Box" 
merge3Box = codeBox "diffMerge3Box" 

codeBox :: String -> HTML -> HTML
codeBox _class h = H.pre ! A.class_ (H.stringValue _class) $ H.code $ h

styleHead, styleEdit, styleOrig :: HTML -> HTML
styleHead = H.span ! A.class_ "diffHead"
styleEdit = H.span ! A.class_ "diffEdit"
styleOrig = H.span ! A.class_ "diffOrig"

styleConflict, barrierConflict :: HTML -> HTML
styleConflict = H.span ! A.class_ "diffConflict" 
barrierConflict = H.span ! A.class_ "diffConflictSep"

-- | Report string-level conflicts for origin, head, and edit.
reportConflicts :: String -> String -> String -> HTML
reportConflicts sOrig sHead sEdit = do
    H.p $ "2-Way Merge (Origin → Head):"
    merge2Box $ twoWayMerge sOrig sHead
    H.p $ "2-Way Merge (Head → Edit):"
    merge2Box $ twoWayMerge sHead sEdit
    H.p $ "3-Way String Merge (Head, Origin, and Edit):"
    merge3Box $ threeWayMerge sHead sOrig sEdit


