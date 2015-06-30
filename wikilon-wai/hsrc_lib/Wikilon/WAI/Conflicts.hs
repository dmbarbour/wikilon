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

headBox, merge2Box, merge3Box :: HTML -> HTML
headBox = codeBox "diffHeadBox" "border-color:Navy;border-style:dashed;border-width:thin" 
merge2Box = codeBox "diffMerge2Box" "border-color:Indigo;border-style:dashed;border-width:thin"
merge3Box = codeBox "diffMerge3Box" "border-color:SeaGreen;border-style:dashed;border-width:thin"

codeBox :: String -> String -> HTML -> HTML
codeBox _class _style h =
    H.pre ! A.style (H.stringValue _style) $ 
    H.code ! A.class_ (H.stringValue _class) ! A.lang "abc" $ h

styleHead, styleEdit, styleOrig :: HTML -> HTML
styleHead = H.span ! A.class_ "diffHead" ! A.style "background-color:DarkSeaGreen"
styleEdit = H.span ! A.class_ "diffEdit" ! A.style "background-color:Thistle"
styleOrig = id

styleConflict, barrierConflict :: HTML -> HTML
styleConflict = H.span ! A.class_ "diffConflict" ! A.style "border-color:DarkOrange;border-style:solid;border-width:medium"
barrierConflict = H.span ! A.class_ "diffConflictSep" ! A.style "color:DarkOrange;font-weight:bolder"

-- | Report string-level conflicts for origin, head, and edit.
reportConflicts :: String -> String -> String -> HTML
reportConflicts sOrig sHead sEdit = do
    H.b "Head Version:"
    headBox $ H.string sHead
    H.b "2-Way String Merge (Head and Edit):"
    merge2Box $ twoWayMerge sHead sEdit
    H.b "3-Way String Merge (Head, Origin, and Edit):"
    merge3Box $ threeWayMerge sHead sOrig sEdit


