{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LU8
import qualified System.EasyFile as FS
import qualified Wikilon.DB as DB
import qualified Data.List as L
import Control.Exception
import Debug.Trace
import System.Mem

-- TODO: learn and use a Haskell test framework.
    
main :: IO ()
main = withTmpDir "wikilon-test" $ do
    (Right db) <- DB.open "test-db" 1000
    testDB db






-- utilities to simplify the read-write labor

utf8 :: Show a => a -> LU8.ByteString
utf8 = LU8.fromString . show

-- just take the first value read
writeBatch :: (Show a, Show b) => DB.DB -> [(a,b)] -> IO ()
writeBatch db kvList = do
    tx <- DB.newTX db
    mapM_ (uncurry (write tx)) kvList
    ok <- DB.commit tx
    unless ok $ fail ("failed to write batch " ++ show kvList)
    return ()

write :: (Show a, Show b) => DB.TX -> a -> b -> IO ()
write tx a b = DB.writeKey tx (utf8 a) (utf8 b)

tryRead :: (Read a) => LU8.ByteString -> a
tryRead s = case reads (LU8.toString s) of
    ((a,_):_) -> a
    [] -> error $ "could not read: " ++ show s

read :: (Show a, Read b) => DB.TX -> a -> IO b
read tx = fmap tryRead . DB.readKey tx . utf8

readDB :: (Show a, Read b) => DB.DB -> a -> IO b
readDB db = fmap tryRead . DB.readKeyDB db . utf8

testDB :: DB.DB -> IO ()
testDB db = do
    -- NOTE: I'll be able to test the database more thoroughly
    -- once I have some support for large tries or similar. 

    tx0 <- DB.newTX db
    DB.writeKey tx0 "red" "green"
    DB.writeKey tx0 "blue" "orange"
    DB.writeKey tx0 "yellow" "violet"
    DB.commit tx0

    r <- DB.readKeyDB db "red"
    tx1 <- DB.newTX db
    b <- DB.readKey tx1 "blue"
    y <- DB.readKey tx1 "yellow"
    DB.commit tx1
    rby <- DB.readKeysDB db ["red","blue","yellow"]

    unless ((r == "green") && (y == "violet") && (rby == [r,b,y])) $
        fail "unexpected results for rby"

    tx2 <- DB.newTX db
    h0 <- DB.stowRsc tx2 "hello"
    h1 <- DB.stowRsc tx2 "goodbye"
    DB.writeKey tx2 "hello" (LBS.fromStrict h1)
    DB.clearRsc tx2
    DB.commit tx2

    let ns = [0..100]
    let sel = L.take 20 . L.drop 30
    tx3 <- DB.newTX db
    hs <- mapM (DB.stowRsc tx3 . utf8) ns
    DB.writeKey tx3 "p" (utf8 (sel hs))
    DB.commit tx3
    DB.clearRsc tx3
    DB.gcDB_async db
    --DB.gcDB db
    -- traceIO ("hs: " ++ show hs)
    ns2 <- mapM (DB.loadRscDB db) hs 
    traceIO ("ns2 : " ++ show ns2)


    hs' <- DB.readKeyDB db "p"
    unless (utf8 (sel hs) == hs') $
        fail ("failure to read expected hashes")



withTmpDir :: FilePath -> IO a -> IO a
withTmpDir subdir action = do
    initialPath <- FS.getCurrentDirectory
    tmp <- FS.getTemporaryDirectory
    let myTmpDir = tmp FS.</> subdir
    FS.createDirectoryIfMissing False myTmpDir  
    FS.setCurrentDirectory myTmpDir
    r <- try action
    FS.setCurrentDirectory initialPath
    -- FS.removeDirectoryRecursive myTmpDir
    reraise r


reraise :: Either SomeException a -> IO a
reraise = either throw return


