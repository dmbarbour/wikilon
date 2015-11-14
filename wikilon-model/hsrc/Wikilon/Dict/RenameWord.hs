{-# LANGUAGE OverloadedStrings #-}

-- | Safely rename a word in the dictionary.
module Wikilon.Dict.RenameWord
    ( safeRenameWord
    , conditionsForSafeRename
    ) where

import Control.Exception (assert)
import Data.Maybe (isNothing, isJust)
import qualified Data.List as L
import Wikilon.Dict.Head

conditionsForSafeRename :: String
conditionsForSafeRename =
    "We rename an 'origin' word to 'target'. This is acceptable if the\n\
    \target word is not defined and not in use, or if the origin and\n\
    \target share identical definitions, or if either word is a redirect\n\
    \to the other. After rename, references to the origin are updated\n\
    \to refer to the target, the origin is undefined, and the target has\n\
    \the definition of the origin (unless origin was the redirect)."

-- Test whether a given definition is a redirect to a given word...
isRedirectTo :: AODef -> Word -> Bool
isRedirectTo = maybe (const False) (==) . aodefRedirect

-- | see conditionsForSafeRename; try to rename a word without damaging
-- the current dictionary structure. This assumes that both the origin
-- and target words are valid.
safeRenameWord :: Word -> Word -> DictHead -> Maybe Dict
safeRenameWord origin target dh =
    let d0 = dhDict dh in

    -- identity rename is safe and trivial, so short circuit
    if (origin == target) then Just d0 else

    let defOrigin = dictLookup d0 origin in
    let defTarget = dictLookup d0 target in
    let clientsOrigin = rluWordClients (dhRLU dh) origin in
    let clientsTarget = rluWordClients (dhRLU dh) target in

    -- safe rename conditions (any one of these is sufficient)
    let bIdenticalDefinitions = isJust defOrigin && (defOrigin == defTarget) in
    let bOriginRedirectsToTarget = maybe False (`isRedirectTo` target) defOrigin in
    let bTargetRedirectsToOrigin = maybe False (`isRedirectTo` origin) defTarget in
    let bUndefAndUnusedTarget = isNothing defTarget && L.null clientsTarget in

    let bSafeRename = 
            bIdenticalDefinitions ||
            bOriginRedirectsToTarget || 
            bTargetRedirectsToOrigin ||
            bUndefAndUnusedTarget 
    in
    if not bSafeRename then Nothing else

    -- otherwise: 
    --   update clients of origin to instead reference target
    --   delete the origin word 
    --   update the target word if necessary
    let delOriginWord = (`dictDelete` origin) in
    let updTargetWord = 
            if (bIdenticalDefinitions || bOriginRedirectsToTarget) 
                then id 
                else case defOrigin of
                    Just def -> \ d -> dictInsert d target def 
                    Nothing -> (`dictDelete` target) -- 
    in
    let rn = renameInAODef origin target in
    let rnOriginClient d wc = case dictLookup d0 wc of
            Just def -> dictInsert d wc (rn def) 
            Nothing -> impossible d -- impossible condition
    in
    let updClientWords d = L.foldl' rnOriginClient d clientsOrigin in
    (Just . delOriginWord . updTargetWord . updClientWords) d0

-- an impossible condition, assuming the dictionary is in good shape.
impossible :: a -> a
impossible = assert False

