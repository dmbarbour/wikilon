module Stowage.Tests

#nowarn "988"

open System
open System.IO
open Xunit
open Stowage.Hash
open Data.ByteString

[<Fact>]
let ``hash test`` () =
    let h0 = BS.fromString "test"
    let h1 = hash h0
    let h2 = hash h1
    let h3 = hash h2
    Assert.Equal<string>(BS.toString h1, "HSjFNGRnqHpFFbPhlThmqCbqkmDSHCBlJNnmDPnDtnCpKHqtNgqhRMJG")
    Assert.Equal<string>(BS.toString h2, "BRqMkFknGGncjKTdrTGMjFFHlGlFmmGGNmcFGPSmGbstsLtpdJnhLNKS")
    Assert.Equal<string>(BS.toString h3, "NLsTsGdQrtFLfDtHJcmqDSmMsDRjnMpCFlkqGfLdgSRhFtTsGqhJrfNN")

let clearTestDir path =
    if Directory.Exists(path) 
        then Directory.Delete(path,true)

// a fixture is needed to load the database
type TestDB =
    val db : Stowage.DB
    new () =
        let path = "testDB"
        let maxSizeMB = 1000
        do clearTestDir path
        { db = openDB path maxSizeMB }
    interface System.IDisposable with
        member this.Dispose() = closeDB this.db

// Stowage doesn't provide a clean way to wait for full GC, but
// we can sync a few times.
let gcDB (db:DB) : unit =
    syncDB db; syncDB db; syncDB db

type DBTests =
    val db : Stowage.DB
    new (fixture : TestDB) = { db = fixture.db }
    interface IClassFixture<TestDB>

    [<Fact>]
    member t.``resource put and get`` () =
        let tests = List.map BS.fromString ["test"; ""; "foo"; "bar"; "baz"; "qux"]
        let rscs = List.map (stowRscDB t.db) tests
        Assert.Equal<ByteString list>(rscs, List.map hash tests)
        syncDB (t.db)
        let loaded = List.map (loadRscDB t.db) rscs
        Assert.Equal<ByteString list>(loaded, tests)
        List.iter (decrefRscDB t.db) rscs
        syncDB (t.db)

    [<Fact>]
    member t.``basic resource GC`` () =
        let join a b = 
            let s = BS.concat [a; BS.singleton 32uy; b]
            (s, stowRscDB t.db s)
        let (a,ra) = join (BS.fromString "x") (BS.fromString "y")
        let (b,rb) = join ra (BS.fromString "z")
        let (c,rc) = join rb rb
        decrefRscDB t.db rb
        gcDB (t.db) // to force writes, GC
        Assert.Equal<ByteString>(a, loadRscDB t.db ra)
        Assert.Equal<ByteString>(b, loadRscDB t.db rb)
        Assert.Equal<ByteString>(c, loadRscDB t.db rc)
        decrefRscDB t.db rc
        gcDB (t.db) 
        Assert.Equal<ByteString option>(Some a, tryLoadRscDB t.db ra)
        Assert.Equal<ByteString option>(None, tryLoadRscDB t.db rb)
        Assert.Equal<ByteString option>(None, tryLoadRscDB t.db rc)
        decrefRscDB t.db ra
        gcDB (t.db)
        Assert.Equal<ByteString option>(None, tryLoadRscDB t.db ra)
        Assert.Equal<ByteString option>(None, tryLoadRscDB t.db rb)
        Assert.Equal<ByteString option>(None, tryLoadRscDB t.db rc)

    [<Fact>]
    member t.``read and write keys`` () = 
        let kvs = [("a","a-val"); ("b","b-value"); ("c","cccc")]
        using (newTX t.db) (fun tx ->
            let wtx (a,b) = writeKey tx (BS.fromString a) (BS.fromString b)
            List.iter (fun (a,b) -> writeKey tx (BS.fromString a) (BS.fromString b)) kvs
            Assert.True(commit tx))
        let rds1 = List.map (fst >> BS.fromString >> readKeyDB t.db) kvs
        let readKeysL db l = List.ofArray (readKeysDB db (List.toArray l))
        let rds2 = readKeysL (t.db) (List.map (fst >> BS.fromString) kvs)
        Assert.Equal<ByteString list>(rds1, rds2)
        Assert.Equal<ByteString list>(rds1, List.map (snd >> BS.fromString) kvs)

    [<Fact>]
    member t.``key containment and discovery`` () =
        let hasKey k = containsKeyDB (t.db) (BS.fromString k)
        let hasKey' arr k = Array.contains (BS.fromString k) arr
        let kvs = [("x", "x-val"); ("y", "y-value"); ("z", "zzzz"); ("zz", "")]
        let kvm = 
            List.toSeq kvs
                |> Seq.map (fun (a,b) -> (BS.fromString a, BS.fromString b))
                |> Map.ofSeq

        writeKeyDB t.db (BS.fromString "zz") (BS.fromString "exists")
        Assert.True(hasKey "zz")

        writeKeysDB t.db kvm
        Assert.True(hasKey "x")
        Assert.True(hasKey "y")
        Assert.True(hasKey "z")
        Assert.False(hasKey "zz")

        let allKeys = discoverKeysDB t.db None 1000000
        //printf "all keys = %A\n" (Array.map BS.toString allKeys)
        Assert.True(hasKey' allKeys "x")
        Assert.True(hasKey' allKeys "y")
        Assert.True(hasKey' allKeys "z")
        Assert.False(hasKey' allKeys "zz")


    [<Fact>]
    member t.``key-value layer serves as GC roots`` () = 
        let a_val = "a-value"
        let b_val = "b-val"
        let c_val = "ccccccc"
        let hasRsc s = 
            let b = BS.fromString s
            let rsc = tryLoadRscDB t.db (hash b)
            //printf "resource %s = %A\n" s (Option.map BS.toString rsc)
            match rsc with
            | None -> false
            | Some v -> Assert.Equal<ByteString>(v,b); true

        using (newTX t.db) (fun tx -> 
            let a_ref = stowRsc tx (BS.fromString a_val)
            let b_ref = stowRsc tx (BS.fromString b_val)
            let c_ref = stowRsc tx (BS.fromString c_val)
            writeKey tx (BS.fromString "a") a_ref
            writeKey tx (BS.fromString "b") b_ref
            Assert.True(commit tx))

        gcDB t.db
        Assert.True(hasRsc a_val)
        Assert.True(hasRsc b_val)
        Assert.False(hasRsc c_val)

        writeKeyDB t.db (BS.fromString "a") BS.empty
        gcDB t.db
        Assert.False(hasRsc a_val)
        Assert.True(hasRsc b_val)
        Assert.False(hasRsc c_val)
        

    [<Fact>]
    member t.``simple transaction conflict`` () = 
        let read tx k = BS.toString (readKey tx (BS.fromString k))
        let write tx k v = writeKey tx (BS.fromString k) (BS.fromString v)
        let assume tx k v = assumeKey tx (BS.fromString k) (BS.fromString v)
        using (newTX t.db) (fun tx ->
            write tx "a" "x"
            write tx "b" "y"
            write tx "c" "z"
            Assert.True(commit tx))
        using (newTX t.db) (fun tx ->
            Assert.Equal(read tx "a", "x")
            Assert.Equal(read tx "b", "y")
            Assert.Equal(None, testReadAssumptions tx.DB tx.Reads)
            assume tx "c" "zz"
            Assert.Equal(read tx "c", "zz")
            Assert.False(commit tx)
            Assert.Equal(Some (BS.fromString "c"), testReadAssumptions tx.DB tx.Reads))


