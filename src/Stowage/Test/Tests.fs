module Stowage.Tests

#nowarn "988"

open System
open System.Threading
open System.IO
open Xunit
open Stowage
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
    val db : DB
    new () =
        let path = "testDB"
        let maxSizeMB = 1000
        do clearTestDir path
        { db = DB.load path maxSizeMB }
    interface System.IDisposable with
        member this.Dispose() = 
            DB.close this.db

// Stowage doesn't provide a clean way to wait for full GC, but
// we can sync a few times to ensure the GC runs a few cycles.
let sync db = DB.sync db
let gcDB (db:DB) : unit = 
    sync db; sync db; sync db

let onPair fn ((a,b)) = (fn a, fn b)
let bsPair p = onPair (BS.fromString) p

type DBTests =
    val db : DB
    new (fixture : TestDB) = { db = fixture.db }
    interface IClassFixture<TestDB>

    [<Fact>]
    member t.``resource put and get`` () =
        let tests = List.map BS.fromString ["test"; ""; "foo"; "bar"; "baz"; "qux"]
        let rscs = List.map (DB.stowRsc t.db) tests
        Assert.Equal<ByteString list>(rscs, List.map hash tests)
        gcDB (t.db)
        let loaded = List.map (DB.loadRsc t.db) rscs
        Assert.Equal<ByteString list>(loaded, tests)
        List.iter (DB.decrefRsc t.db) rscs

    [<Fact>]
    member t.``basic resource GC`` () =
        let join a b = 
            let s = BS.concat [a; BS.singleton 32uy; b]
            (s, DB.stowRsc t.db s)
        let (a,ra) = join (BS.fromString "x") (BS.fromString "y")
        let (b,rb) = join ra (BS.fromString "z")
        let (c,rc) = join rb rb
        DB.decrefRsc t.db rb
        gcDB (t.db) // to force writes, GC
        Assert.Equal<ByteString>(a, DB.loadRsc t.db ra)
        Assert.Equal<ByteString>(b, DB.loadRsc t.db rb)
        Assert.Equal<ByteString>(c, DB.loadRsc t.db rc)
        DB.decrefRsc t.db rc
        gcDB (t.db) 
        Assert.Equal<ByteString option>(Some a, DB.tryLoadRsc t.db ra)
        Assert.Equal<ByteString option>(None, DB.tryLoadRsc t.db rb)
        Assert.Equal<ByteString option>(None, DB.tryLoadRsc t.db rc)
        DB.decrefRsc t.db ra
        gcDB (t.db)
        Assert.Equal<ByteString option>(None, DB.tryLoadRsc t.db ra)
        Assert.Equal<ByteString option>(None, DB.tryLoadRsc t.db rb)
        Assert.Equal<ByteString option>(None, DB.tryLoadRsc t.db rc)

    [<Fact>]
    member t.``read and write keys`` () = 
        let kvs = List.map bsPair [("a","a-val"); ("b","b-value"); ("c","cccc")]
        DB.writeKeys (t.db) (BTree.ofList kvs)
        let rds1 = List.map (fst >> DB.readKey t.db) kvs
        let rds2 = List.ofArray (DB.readKeys (t.db) (List.toArray (List.map fst kvs)))
        Assert.Equal<ByteString list>(rds1, rds2)
        Assert.Equal<ByteString list>(rds1, List.map snd kvs)

    [<Fact>]
    member t.``key containment and discovery`` () =
        let hasKey k = DB.containsKey (t.db) (BS.fromString k)
        let hasKey' arr k = Array.contains (BS.fromString k) arr
        let kvs = List.map bsPair [("x", "x-val"); ("y", "y-value"); ("z", "zzzz"); ("zz", "")]
        let kvm = BTree.ofList kvs

        DB.writeKey (t.db) (BS.fromString "zz") (BS.fromString "exists")
        Assert.True(hasKey "zz")

        DB.writeKeys (t.db) kvm
        Assert.True(hasKey "x")
        Assert.True(hasKey "y")
        Assert.True(hasKey "z")
        Assert.False(hasKey "zz")

        let allKeys = DB.discoverKeys t.db None 1000000
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
            let rsc = DB.tryLoadRsc t.db (hash b)
            //printf "resource %s = %A\n" s (Option.map BS.toString rsc)
            match rsc with
            | None -> false
            | Some v -> Assert.Equal<ByteString>(v,b); true

        let a_ref = DB.stowRsc t.db (BS.fromString a_val)
        let b_ref = DB.stowRsc t.db (BS.fromString b_val)
        let c_ref = DB.stowRsc t.db (BS.fromString c_val)

        DB.writeKeyAsync (t.db) (BS.fromString "a") a_ref
        DB.writeKeyAsync (t.db) (BS.fromString "b") b_ref

        // release local refs to resources
        DB.decrefRsc t.db a_ref
        DB.decrefRsc t.db b_ref
        DB.decrefRsc t.db c_ref

        gcDB t.db
        Assert.True(hasRsc a_val)
        Assert.True(hasRsc b_val)
        Assert.False(hasRsc c_val)

        DB.writeKey t.db (BS.fromString "a") BS.empty
        gcDB t.db
        Assert.False(hasRsc a_val)
        Assert.True(hasRsc b_val)
        Assert.False(hasRsc c_val)
        

    [<Fact>]
    member t.``simple transaction conflict`` () = 
        let a = BTree.ofList (List.map bsPair [("foo","baz")])
        let b = BTree.ofList (List.map bsPair [("foo","bar")])
        DB.writeKeys t.db a

        Assert.False(DB.update t.db b a)
        Assert.True(DB.update t.db a b)
        Assert.False(DB.update t.db a b)
        Assert.True(DB.update t.db b a)

    [<Fact>]
    member t.``cannot decref below zero!``() =
        let rsc = BS.fromString "testing: cannot decref below zero!"
        let ref = DB.stowRsc t.db rsc
        DB.sync t.db
        DB.decrefRsc t.db ref
        Assert.Throws<InvalidOperationException>(fun () -> 
            DB.decrefRsc t.db ref)

    [<Fact>]
    member t.``fast enough for practical work`` () =
        DB.sync (t.db) // avoid pending data

        let sw = System.Diagnostics.Stopwatch()
        sw.Restart()
        let refs = 
            [| for i = 1 to 10000 do 
                let rsc = DB.stowRsc (t.db) (BS.fromString (string i)) 
                yield rsc
            |]
        DB.sync (t.db) // ensure all data is on disk
        sw.Stop()
        let usecPerStow = (sw.Elapsed.TotalMilliseconds * 1000.0) 
                            / (float refs.Length)
        printfn "usec per stowed element: %A" usecPerStow

        /// in environment with a bunch of refs, focus incref/decref on
        /// a few specific references. These should be randomly named due
        /// to the secure hashes. 
        let reps = 1000
        let focus = refs.[300..399]
        sw.Restart()
        for i = 1 to reps do
            Array.iter (DB.increfRsc t.db) focus
        for i = 1 to reps do
            Array.iter (DB.decrefRsc t.db) focus
        sw.Stop()
        let usecPerRep = (sw.Elapsed.TotalMilliseconds * 1000.0) 
                            / (float (reps * focus.Length))
        printfn "usec per incref + decref rep: %A" usecPerRep

        // cleanup, or this might interfere with GC roots test
        Array.iter (DB.decrefRsc t.db) refs
        for i = 0 to 10 do DB.sync t.db // assumes ~1k elements GC'd per step



        Assert.True(usecPerStow < 120.0)
        Assert.True(usecPerRep < 5.0)




