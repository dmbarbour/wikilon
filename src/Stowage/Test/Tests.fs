module Stowage.Tests

#nowarn "988"

open System
open System.Threading
open System.IO
open Xunit
open Stowage
open Data.ByteString

let shuffle' (rng:System.Random) (a : 'T[]) : unit =
    let rec shuffleIx ix =
        if (ix = a.Length) then () else
        let ixSwap = rng.Next(ix, Array.length a)
        let tmp = a.[ix]
        a.[ix] <- a.[ixSwap]
        a.[ixSwap] <- tmp
        shuffleIx (ix + 1)
    shuffleIx 0

let clearTestDir path =
    if Directory.Exists(path) 
        then Directory.Delete(path,true)

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

[<Fact>]
let ``intmap hbi`` () =
    let inline hbi n = int (IntMap.Critbit.highBitIndex n)
    Assert.Equal(0, hbi 0UL)
    Assert.Equal(0, hbi 1UL)
    Assert.Equal(1, hbi 2UL)
    Assert.Equal(1, hbi 3UL)
    Assert.Equal(2, hbi 4UL)
    Assert.Equal(2, hbi 7UL)
    Assert.Equal(3, hbi 8UL)
    Assert.Equal(3, hbi 15UL)
    Assert.Equal(4, hbi 16UL)
    Assert.Equal(4, hbi 31UL)
    Assert.Equal(5, hbi 32UL)
    Assert.Equal(5, hbi 63UL)
    Assert.Equal(7, hbi 128UL)
    Assert.Equal(7, hbi 255UL)
    Assert.Equal(10, hbi 1024UL)
    Assert.Equal(11, hbi 4095UL)
    Assert.Equal(27, hbi ((1UL <<< 28) - 1UL))
    Assert.Equal(63, hbi (1UL <<< 63))
    Assert.Equal(63, hbi System.UInt64.MaxValue)

[<Fact>]
let ``history independent intmap construction`` () =
    let inline add i m = IntMap.add (uint64 i) i m
    let m0 = IntMap.empty
    let m3 = m0 |> add 0 |> add 1 |> add 2
    // exhaustive testing for permutations of 3 items
    Assert.Equal(m3, m0 |> add 0 |> add 1 |> add 2)
    Assert.Equal(m3, m0 |> add 0 |> add 2 |> add 1)
    Assert.Equal(m3, m0 |> add 1 |> add 0 |> add 2)
    Assert.Equal(m3, m0 |> add 1 |> add 2 |> add 0)
    Assert.Equal(m3, m0 |> add 2 |> add 0 |> add 1)
    Assert.Equal(m3, m0 |> add 2 |> add 1 |> add 0)

    // test idempotence of add
    Assert.Equal(m3, m3 |> add 0)
    Assert.Equal(m3, m3 |> add 1)
    Assert.Equal(m3, m3 |> add 2)

    // pseudo-random testing
    let fromI i = (uint64 i, i)
    let fromA a = IntMap.ofSeq (Seq.map fromI (Array.toSeq a))
    let rng = new System.Random(21)
    let a = Array.init 32 id
    let asum = Array.fold (+) 0 a
    let m0 = fromA a
    let msum = IntMap.fold (fun s k v -> (s + v)) 0 m0
    Assert.Equal(asum,msum)
    Assert.True(IntMap.validate m0)
    for i = 1 to 1000 do
        shuffle' rng a
        Assert.Equal(m0, fromA a)

[<Fact>]
let ``intmap removal`` () =
    let inline add i m = IntMap.add (uint64 i) i m
    let inline rm i m = IntMap.remove (uint64 i) m
    let m0 = IntMap.empty
    let m4 = IntMap.empty |> add 0 |> add 1 |> add 2 |> add 3

    // test containment and lookup
    Assert.True(IntMap.containsKey 0UL m4)
    Assert.False(IntMap.containsKey 0UL (m4 |> rm 0))
    Assert.True(IntMap.containsKey 1UL m4)
    Assert.False(IntMap.containsKey 1UL (m4 |> rm 1))
    Assert.True(IntMap.containsKey 2UL m4)
    Assert.False(IntMap.containsKey 2UL (m4 |> rm 2))
    Assert.True(IntMap.containsKey 3UL m4)
    Assert.False(IntMap.containsKey 3UL (m4 |> rm 3))
    Assert.False(IntMap.containsKey 4UL m4)

    // test idempotence of removal
    Assert.Equal(m4 |> rm 0 |> rm 0, m4 |> rm 0)
    Assert.Equal(m4 |> rm 1 |> rm 1, m4 |> rm 1)
    Assert.Equal(m4 |> rm 2 |> rm 2, m4 |> rm 2)
    Assert.Equal(m4 |> rm 3 |> rm 3, m4 |> rm 3)

    // removal of absent value
    Assert.Equal(m4 |> rm 4, m4)

    // exhaustive testing for four items (without ordering)
    Assert.Equal(m4 |> rm 0, m0 |> add 1 |> add 2 |> add 3)
    Assert.Equal(m4 |> rm 1, m0 |> add 0 |> add 2 |> add 3)
    Assert.Equal(m4 |> rm 2, m0 |> add 0 |> add 1 |> add 3)
    Assert.Equal(m4 |> rm 3, m0 |> add 0 |> add 1 |> add 2)
    Assert.Equal(m4 |> rm 0 |> rm 1, m0 |> add 2 |> add 3)
    Assert.Equal(m4 |> rm 0 |> rm 2, m0 |> add 1 |> add 3)
    Assert.Equal(m4 |> rm 0 |> rm 3, m0 |> add 1 |> add 2)
    Assert.Equal(m4 |> rm 1 |> rm 2, m0 |> add 0 |> add 3)
    Assert.Equal(m4 |> rm 1 |> rm 3, m0 |> add 0 |> add 2)
    Assert.Equal(m4 |> rm 2 |> rm 3, m0 |> add 0 |> add 1)
    Assert.Equal(m4 |> rm 0 |> rm 1 |> rm 2, m0 |> add 3)
    Assert.Equal(m4 |> rm 0 |> rm 1 |> rm 3, m0 |> add 2)
    Assert.Equal(m4 |> rm 0 |> rm 2 |> rm 3, m0 |> add 1)
    Assert.Equal(m4 |> rm 1 |> rm 2 |> rm 3, m0 |> add 0)
    Assert.Equal(m4 |> rm 0 |> rm 1 |> rm 2 |> rm 3, m0)

    // testing for order of removal
    Assert.Equal(m4 |> rm 1 |> rm 0, m0 |> add 2 |> add 3)
    Assert.Equal(m4 |> rm 2 |> rm 0, m0 |> add 1 |> add 3)
    Assert.Equal(m4 |> rm 3 |> rm 0, m0 |> add 1 |> add 2)
    Assert.Equal(m4 |> rm 2 |> rm 1, m0 |> add 0 |> add 3)
    Assert.Equal(m4 |> rm 3 |> rm 1, m0 |> add 0 |> add 2)
    Assert.Equal(m4 |> rm 3 |> rm 2, m0 |> add 0 |> add 1)

    // random testing   
    let a = Array.init 32 id
    let fromA a = Array.toSeq a |> Seq.map (fun i -> (uint64 i, i)) |> IntMap.ofSeq
    let valsA m = IntMap.toSeq m |> Seq.map snd |> Array.ofSeq
    let msum m = IntMap.fold (fun s k v -> (s + v)) 0 m
    Assert.Equal(msum (fromA a), Array.fold (+) 0 a)
    let rng = new System.Random(22)
    for i = 1 to 1000 do
        shuffle' rng a
        let mr = fromA a |> rm a.[0] |> rm a.[1] |> rm a.[2]
        let mx = fromA (a.[3..])
        Assert.Equal(mr,mx)

[<Fact>]
let ``history independent trie construction`` () =
    let a = Array.init 1000 id
    let fromI i = (BS.fromString (string i), i)
    let fromA a = Array.toSeq a |> Seq.map fromI |> Trie.ofSeq
    let sumV t = Trie.foldBack (fun k v s -> (s + v)) t 0
    let rng = new System.Random(1111)
    let t0 = fromA a
    let sum0 = sumV t0
    Assert.Equal(sum0, Array.fold (+) 0 a)
    for i = 1 to 100 do
        shuffle' rng a
        let t = fromA a
        Assert.Equal(t0,t)

[<Fact>]
let ``trie access and removal`` () =
    let t = seq { for i = 1 to 2000 do yield i }
                |> Seq.map (fun i -> (BS.fromString (string i), i))
                |> Trie.ofSeq
    let lu i t = Trie.tryFind (BS.fromString (string i)) t
    let rm i t = Trie.remove (BS.fromString (string i)) t
    Assert.Equal(None, lu 0 t)
    Assert.Equal(Some 1, lu 1 t)
    Assert.Equal(Some 2, lu 2 t)
    Assert.Equal(Some 20, lu 20 t)
    Assert.Equal(Some 100, lu 100 t)
    Assert.Equal(Some 200, lu 200 t)
    Assert.Equal(Some 1000, lu 1000 t)
    Assert.Equal(Some 2000, lu 2000 t)
    Assert.Equal(None, lu 3000 t)
    Assert.Equal(None, lu 1 (rm 1 t))
    Assert.Equal(Some 100, lu 100 (t |> rm 1 |> rm 10 |> rm 11))

    let t2 = Trie.filterPrefix (BS.fromString "10") t
    Assert.Equal(None, lu 1 t2)
    Assert.Equal(None, lu 2 t2)
    Assert.Equal(Some 10, lu 10 t2)
    Assert.Equal(None, lu 20 t2)
    Assert.Equal(Some 101, lu 101 t2)
    Assert.Equal(None, lu 110 t2)
    



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

    [<Fact>]
    member t.``vref basics`` () =
        let cv = EncStringRaw.codec
        let ss = ["hello"; "world"; "this"; "is"; "a"; "test"]
        let vrefs = List.map (VRef.stow cv t.Stowage) ss
        t.FullGC()
        let ss' = List.map (VRef.load) vrefs
        Assert.Equal<string list>(ss,ss')
        //printfn "%A" vrefs

    [<Fact>]
    member t.``cvref basics`` () =
        let stow s = CVRef.stow 10 (EncStringRaw.codec) (t.Stowage) s
        let a = stow "hello"
        let b = stow "hello, world!"
        //printfn "a=%A; b=%A" a b
        Assert.False(CVRef.isRemote a)
        Assert.True(CVRef.isRemote b)


    [<Fact>]
    member t.``intmap serialization`` () =
        let mutable m = IntMap.empty
        for i = 1 to 50 do
            let k = uint64 (2 * i)
            m <- IntMap.add k k m
        for i = 1 to 50 do
            let k = uint64 ((2 * i) - 1)
            m <- IntMap.add k k m
        let cm = IntMap.codec' (System.Int32.MaxValue) (EncVarNat.codec) 
        let struct(mCompact,szM) = Codec.compactSz cm (t.Stowage) m
        let mbytes = Codec.writeBytes cm mCompact
        Assert.Equal(BS.length mbytes, szM) // require exact size estimate
        t.FullGC()
        let m' = Codec.readBytes cm (t.Stowage) mbytes
        Assert.Equal(100, Seq.length (IntMap.toSeq m))
        Assert.Equal<(uint64 * uint64) seq>(IntMap.toSeq m, IntMap.toSeq m')

    [<Fact>]
    member t.``intmap compaction`` () =
        let s = seq { for i = 1 to 2000 do yield i }
        let fromI i = (uint64 i, i)
        let ss = Seq.map fromI s
        let m = IntMap.ofSeq ss
        let cm = IntMap.codec' 400 (EncVarInt32.codec) 
        let struct(mCompact,szM) = Codec.compactSz cm (t.Stowage) m
        //printfn "compacted to: %d" szM
        Assert.True(szM < 500)
        Assert.Equal(Some 7, IntMap.tryFind 7UL mCompact)
        Assert.Equal(Some 201, IntMap.tryFind 201UL mCompact)
        Assert.Equal(Some 999, IntMap.tryFind 999UL mCompact)
        Assert.Equal(None, IntMap.tryFind 3000UL mCompact)
        let mbytes = Codec.writeBytes cm mCompact
        Assert.Equal(szM, BS.length mbytes)
        t.FullGC()
        let m' = Codec.readBytes cm (t.Stowage) mbytes
        Assert.Equal(2000, Seq.length (IntMap.toSeq m))
        Assert.Equal<(uint64 * int) seq>(IntMap.toSeq m, IntMap.toSeq m')

    // TODO: Test data structures.


