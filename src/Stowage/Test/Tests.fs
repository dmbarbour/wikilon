module Stowage.Tests

#nowarn "988"

open System
open System.Threading
open System.IO
open Xunit
open Stowage
open Data.ByteString

[<Fact>]
let ``hash test`` () =
    let h0s = "test"
    let h1s = "rmqJNQQmpNmKlkRtsbjnjdmbLQdpKqNlndkNKKpnGDLkmtQLPNgBBQTRrJgjdhdl"
    let h2s = "cctqFDRNPkprCkMhKbsTDnfqCFTfSHlTfhBMLHmhGkmgJkrBblNTtQhgkQGQbffF"
    let h3s = "bKHFQfbHrdkGsLmGhGNqDBdfbPhnjJQjNmjmgHmMntStsNgtmdqmngNnNFllcrNb"
    let h0 = BS.fromString h0s
    let h1 = RscHash.hash h0
    let h2 = RscHash.hash h1
    let h3 = RscHash.hash h2
    Assert.Equal<string>(BS.toString h1, h1s)     
    Assert.Equal<string>(BS.toString h2, h2s)
    Assert.Equal<string>(BS.toString h3, h3s)

let clearTestDir path =
    if Directory.Exists(path) 
        then Directory.Delete(path,true)

// a fixture is needed to load the database
type TestDB =
    val s : LMDB.Storage
    val db : DB
    new () =
        let path = "testDB"
        let maxSizeMB = 1000
        do clearTestDir path
        let s = new LMDB.Storage(path,maxSizeMB) 
        { s = s 
          db = DB.fromStorage (s :> DB.Storage)
        }
    interface System.IDisposable with
        member this.Dispose() = 
            this.db.Flush()
            (this.s :> System.IDisposable).Dispose()

let bsPair ((a,b)) = (BS.fromString a, BS.fromString b)

type DBTests =
    val s  : LMDB.Storage
    val db : DB
    new (fixture : TestDB) = 
        let s = fixture.s
        { s = fixture.s
          db = fixture.db
        }
    interface IClassFixture<TestDB>

    member inline t.DB with get() = t.db
    member inline t.Stowage with get() = (t.s :> Stowage)
    member inline t.Storage with get() = (t.s :> DB.Storage)
    member inline t.Flush() = DB.flushStorage (t.Storage)

    member t.TryLoad (h:RscHash) : ByteString option =
        try t.Stowage.Load h |> Some
        with
            | MissingRsc _ -> None

    member t.FullGC() = 
        System.GC.Collect()
        let rec gcLoop ct =
            t.s.GC()
            let ct' = t.s.Stats().stow_count
            //printfn "GC - elements in stowage: %A" ct'
            if (ct' <> ct) then gcLoop ct'
        gcLoop 0UL

    [<Fact>]
    member t.``resource put and get`` () =
        let tests = List.map BS.fromString ["test"; ""; "foo"; "bar"; "baz"; "qux"]
        let rscs = List.map (t.Stowage.Stow) tests
        Assert.Equal<ByteString list>(rscs, List.map (RscHash.hash) tests)
        let loaded_preflush = List.map (t.Stowage.Load) rscs
        Assert.Equal<ByteString list>(loaded_preflush, tests)
        t.Flush()
        let loaded = List.map (t.Stowage.Load) rscs
        Assert.Equal<ByteString list>(loaded, tests)
        List.iter (t.Stowage.Decref) rscs
        t.FullGC()


    [<Fact>]
    member t.``basic resource GC`` () =
        let join a b = 
            let s = BS.concat [a; BS.singleton 32uy; b]
            (s, t.Stowage.Stow s)
        let (a,ra) = join (BS.fromString "x") (BS.fromString "y")
        let (b,rb) = join ra (BS.fromString "z")
        let (c,rc) = join rb rb
        t.Stowage.Decref rb
        t.FullGC()
        Assert.Equal<ByteString>(a, t.Stowage.Load ra)
        Assert.Equal<ByteString>(b, t.Stowage.Load rb) // held by rc
        Assert.Equal<ByteString>(c, t.Stowage.Load rc)
        t.Stowage.Decref rc
        t.FullGC()
        Assert.Equal<ByteString option>(Some a, t.TryLoad ra)
        Assert.Equal<ByteString option>(None, t.TryLoad rb)
        Assert.Equal<ByteString option>(None, t.TryLoad rc)
        t.Stowage.Decref ra
        t.FullGC()
        Assert.Equal<ByteString option>(None, t.TryLoad ra)

    member t.ToKey (s:string) : DB.Key = 
        t.Storage.Mangle (BS.fromString s)
    member t.ToVal (s:string) : DB.Val =
        if (0 = String.length s) then None else
        Some (BS.fromString s)
    member t.KVP ((k,v)) = (t.ToKey k, t.ToVal v)

    [<Fact>]
    member t.``read and write keys`` () = 
        let kvs = List.map (t.KVP) [("a","a-val"); ("b","b-value"); ("c","cccc")]
        let vs = List.map snd kvs
        let sync = t.Storage.WriteBatch (BTree.ofList kvs)
        let rd1 = List.map (fst >> t.Storage.Read) kvs
        Assert.Equal<DB.Val list>(rd1,vs)
        sync.Force() 
        let rd2 = List.map (fst >> t.Storage.Read) kvs
        Assert.Equal<DB.Val list>(rd2,vs)

    member t.HasRsc (b:ByteString) =
        match t.TryLoad (RscHash.hash b) with
        | None -> false
        | Some v -> Assert.Equal<ByteString>(v,b); true
        

    [<Fact>]
    member t.``key-value layer serves as GC roots`` () = 
        let a_val = "a-value"
        let b_val = "b-val"
        let c_val = "ccccccc"
        let hasRsc s = t.HasRsc (BS.fromString s)

        let a_ref = t.Stowage.Stow (BS.fromString a_val)
        let b_ref = t.Stowage.Stow (BS.fromString b_val)
        let c_ref = t.Stowage.Stow (BS.fromString c_val)

        let writeAsync ks v = 
            let k = BS.fromString ks
            t.Storage.WriteBatch (BTree.singleton k (Some v)) 
                |> ignore<DB.Sync>
        writeAsync "a" a_ref
        writeAsync "b" b_ref
        t.Stowage.Decref a_ref
        t.Stowage.Decref b_ref
        t.Stowage.Decref c_ref
        t.FullGC()
        Assert.True(hasRsc a_val)
        Assert.True(hasRsc b_val)
        Assert.False(hasRsc c_val)
        writeAsync "a" BS.empty
        t.FullGC()
        Assert.False(hasRsc a_val)
        Assert.True(hasRsc b_val)
        Assert.False(hasRsc c_val)


    [<Fact>]
    member t.``cannot decref below zero!``() =
        let rsc = BS.fromString "testing: cannot decref below zero!"
        let ref = t.Stowage.Stow rsc
        t.Flush()
        for i = 1 to 10000 do
            t.Storage.Incref ref
        for i = 0 to 10000 do // one extra decref due to implicit from stowRsc
            t.Storage.Decref ref
        Assert.Throws<InvalidOperationException>(fun () -> 
            t.Storage.Decref ref)

    [<Fact>]
    member t.``fast enough for practical work`` () =
        t.FullGC()

        // don't want to pay for resource construction, and DO want to
        // test resources of moderate to large sizes, so just slicing
        // from rscBytes for between 200 and 1000 bytes. 
        let rscBytes = // pseudo-random bytes
            let src = new System.Random(11)
            let rb (_ : int) = byte (src.Next(256))
            BS.unsafeCreateA (Array.init 10000 rb)
        let maxRscLen = 1000
        let rsc i = // slices of rscBytes
            let sz = 100 + (i % 900)
            BS.take sz (BS.drop i rscBytes)

        let sw = System.Diagnostics.Stopwatch()
        sw.Restart()
        let refs = 
            [| for i = 0 to (rscBytes.Length - maxRscLen) do 
                yield (t.Storage.Stow (rsc i))
            |]
        t.Flush()
        sw.Stop()
        let usecPerStow = (sw.Elapsed.TotalMilliseconds * 1000.0) 
                            / (float refs.Length)
        printfn "usec per stowed element: %A" usecPerStow

        /// Lookup performance.
        sw.Restart()
        for i = 0 to (refs.Length - 1) do
            Assert.Equal<ByteString>(rsc i, t.Storage.Load (refs.[i]))
        sw.Stop()
        let usecPerLookup = (sw.Elapsed.TotalMilliseconds * 1000.0)
                                / (float refs.Length)
        printfn "usec per resource lookup: %A" usecPerLookup

        /// in environment with a bunch of refs, focus incref/decref on
        /// a few specific references. These should be randomly named due
        /// to the secure hashes. 
        let reps = 1000
        let focus = refs.[300..399]
        sw.Restart()
        for i = 1 to reps do
            Array.iter (t.Storage.Incref) focus
        for i = 1 to reps do
            Array.iter (t.Storage.Decref) focus
        sw.Stop()
        let usecPerRep = (sw.Elapsed.TotalMilliseconds * 1000.0) 
                            / (float (reps * focus.Length))
        printfn "usec per incref + decref rep: %A" usecPerRep

        // cleanup, or this might interfere with GC roots test
        //  this interference happens due to concurrent GC.
        Array.iter (t.Storage.Decref) refs
        t.FullGC()

        // guard against performance regressions!
        Assert.True(usecPerStow < 200.0) // ~55 on my machine
        Assert.True(usecPerLookup < 50.0) // ~13 on my machine
        Assert.True(usecPerRep < 5.0)    // ~1.3 on my machine

    [<Fact>] 
    member t.``ephemeral variables`` () =
        let a = t.DB.Allocate "a"
        let b = t.DB.Allocate "b"
        Assert.Equal<string>("a", t.DB.Read a)
        Assert.Equal<string>("b", t.DB.Read b)
        t.DB.Flush()
        t.DB.Write a "a'"
        Assert.Equal<string>("a'", t.DB.Read a)
        Assert.Equal<string>("b", t.DB.Read b)

    [<Fact>]    
    member t.``ephemeral tx conflict and snapshot isolation`` () =
        let a = t.DB.Allocate "a"
        let b = t.DB.Allocate "b"
        let struct(_,tx2_commit) = t.DB.Transact(fun tx ->
            let struct(_,tx1_commit) = t.DB.Transact (fun tx -> 
                tx.Write a "a1")
            Assert.Equal<string>("a", tx.Read a) // isolated from tx1
            tx.Write b "b2" 
            Assert.Equal<string>("b2", tx.Read b)
            Assert.True(tx1_commit)
            ())
        Assert.False(tx2_commit)
        Assert.Equal<string>("a1", t.DB.Read a)
        Assert.Equal<string>("b", t.DB.Read b)

    [<Fact>]
    member t.``durable variable read-write`` () =
        let ka = BS.fromString "durable variables: a"
        let w1 = "hello"
        let struct(_,tx1_ok) = t.DB.Transact(fun tx ->
            let a = tx.Register ka (EncString.codec)
            Assert.Equal<string option>(None, tx.Read a)
            tx.Write a (Some w1)
            tx.Flush())
        Assert.True(tx1_ok)
        t.FullGC()
        t.FullGC()
        t.FullGC()
        let struct(_,tx2_ok) = t.DB.Transact(fun tx ->
            let a = tx.Register ka (EncString.codec)
            Assert.Equal<string option>(Some w1, tx.Read a)
            tx.Write a None
            tx.Flush())
        Assert.True(tx2_ok)
        t.FullGC()

    [<Fact>]
    member t.``equivalence of compatible registrations`` () =
        let ka = BS.fromString "compat reg: a"
        let w1 = Some "hello"
        let va1 = t.DB.Register ka (EncString.codec)
        t.DB.Write va1 w1
        t.DB.Flush()
        let va2 = t.DB.Register ka (EncString.codec)
        Assert.Equal<string option>(w1, t.DB.Read va2)
        Assert.Same(va1,va2)

    [<Fact>]
    member t.``failure of incompatible registrations`` () =
        let ka = BS.fromString "incompat reg: a"
        let va1 = t.DB.Register ka (EncVarNat.codec)
        Assert.Throws<InvalidOperationException>(fun () ->
            t.DB.Register ka (EncString.codec) |> ignore)


    [<Fact>]    
    member t.``durable tx conflict`` () =
        let a = t.DB.Register (BS.fromString "durtxc-a") (EncString.codec)
        let b = t.DB.Register (BS.fromString "durtxc-b") (EncString.codec)
        let struct(_,tx2_commit) = t.DB.Transact(fun tx ->
            let struct(_, tx1_commit) = t.DB.Transact(fun tx ->
                tx.Write a (Some "a1")
                tx.Flush())
            Assert.Equal<string option>(None, tx.Read a)
            tx.Write b (Some "b2") 
            Assert.Equal<string option>(Some "b2", tx.Read b)
            Assert.True(tx1_commit)
            tx.Flush())
        Assert.False(tx2_commit)
        Assert.Equal<string option>(Some "a1", t.DB.Read a)
        Assert.Equal<string option>(None, t.DB.Read b)


    // TODO: Test data structures.


