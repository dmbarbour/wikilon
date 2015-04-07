{-# LANGUAGE OverloadedStrings #-}

-- simple testing environment for ghci
module Env 
    ( vc, theDict, ls, ins, mv, rm, cat, uby, lx
    , help
    , module ABC.Basic
    , module Database.VCache
    ) where

import Control.Applicative 
import Control.Monad
import System.IO.Unsafe
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Wikilon.Dict as D
import qualified Wikilon.Dict.Export as D
import ABC.Basic
import Database.VCache

vc :: VCache
vc = unsafePerformIO (openVCache 1000 "db")
{-# NOINLINE vc #-}

help :: IO ()
help = putStrLn helpMsg where

helpMsg :: String
helpMsg =
 "ls     list all words in the dictionary\n\
 \lx     list words in dictionary for export\n\
 \cat    print definition for a word\n\
 \mv     rename a word\n\
 \ins    insert a word\n\
 \rm     delete a word\n\
 \uby    reverse lookup clients of word\n\
 \stat   report VCache stats\n\
 \"

theDict :: PVar D.Dict
theDict = loadRootPVar vc "dict" (D.empty (vcache_space vc))

ls :: IO [(D.Word, ABC)]
ls = D.toList <$> readPVarIO theDict

lx :: IO String
lx = _toString <$> readPVarIO theDict where
    _toString = LazyUTF8.toString . D.encode

cat :: D.Word -> IO (Maybe ABC)
cat w = 
    readPVarIO theDict >>= \ d ->
    return (D.lookup d w)

stat :: IO VCacheStats
stat = vcacheStats (vcache_space vc)

nop :: Monad m => m ()
nop = return ()

mv :: D.Word -> D.Word -> IO ()
mv wo wt = join $ runVTx (vcache_space vc) $ do
    d0 <- readPVar theDict
    case D.rename1 d0 wo wt of
        Nothing -> return $ putStrLn "rename failed"
        Just df -> writePVar theDict df >> return nop

ins :: D.Word -> ABC -> IO ()
ins w abc = join $ runVTx (vcache_space vc) $ do
    d0 <- readPVar theDict
    case D.insert d0 [(w,abc)] of
        Left errs -> return $ putStrLn $ "insert failed: " ++ show errs
        Right df -> writePVar theDict df >> return nop

rm :: D.Word -> IO ()
rm w = join $ runVTx (vcache_space vc) $ do
    d0 <- readPVar theDict
    case D.delete d0 [w] of
        Left uws -> return $ putStrLn $ "delete failed, dependents: " ++ show uws
        Right df -> writePVar theDict df >> return nop

uby :: D.Word -> IO [D.Word]
uby w = 
    readPVarIO theDict >>= \ d ->
    return (D.usedBy d w)
