module Tests

open System
open System.IO
open Xunit
open Stowage
open Awelon
open Data.ByteString

// For parser tests, it's convenient to parse then print and
// compare the printed string. If parse fails, we can instead
// test the remainder with "?rem".
let ps s =
    match Parser.parse (BS.fromString s) with
    | Parser.ParseOK p -> BS.toString (Parser.write p)
    | Parser.ParseFail st -> BS.toString (BS.cons (byte '?') (st.s))

// Shuffle an array for various tests.
let shuffle (rng:System.Random) (a : 'T[]) : unit =
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

let readNat (s:ByteString) : int =
    let readByte acc c = (acc * 10) + int (c - byte '0')
    BS.fold readByte 0 s

[<Fact>]
let ``basic parser tests`` () =
    Assert.Equal("", ps "    ")
    Assert.Equal("1 2 3", ps " 1  2   3    ")
    Assert.Equal("\"hello\" world", ps "  \"hello\"    world   ")
    Assert.Equal("?\"test", ps " 1 2 \"hello\"  \"test")
    Assert.Equal("[[] [[] []]] []", ps "[[][[][]]][]")
    Assert.Equal("?@ ", ps "1 2 3 @ ")
    Assert.Equal("?→", ps "→")
    Assert.Equal("?", ps "[0[1[2")
    Assert.Equal("?]]]", ps "[]0]]]")
    Assert.Equal("hello/world/[1]", ps "  hello/world/[  1  ]  ")
    Assert.Equal("a/b/c/d", ps "  a/b/c/d  ")
    Assert.Equal("? []", ps "hello/world/ []")
    Assert.Equal("(hello)", ps "  (hello)  ")
    Assert.Equal("?(hel ", ps "  (hel ")
    Assert.Equal("?) ", ps "  hello) ")
    Assert.Equal("?%bbb", ps "  %bbb")
    let h = RscHash.hash (BS.fromString "test")
    let asBin = BS.toString (BS.cons (byte '%') h)
    let asRsc = BS.toString (BS.cons (byte '$') h)
    Assert.Equal(asBin, ps asBin)
    Assert.Equal(asRsc, ps asRsc)

// TODO: test interpreters


let inline bs n = BS.fromString (string n)
let inline addS s d = Dict.add s (Dict.Def(s)) d
let inline addN (n:int) (d:Dict) : Dict = addS (bs n) d
let inline remN (n:int) (d:Dict) : Dict = Dict.remove (bs n) d
let inline flip fn a b = fn b a

[<Fact>]
let ``simple dict tests`` () =
    Assert.True(Dict.isEmpty (Dict.empty))
    Assert.False(Dict.isEmpty (Dict.empty |> addN 1))
    Assert.True(Dict.isEmpty (Dict.empty |> addN 1 |> remN 1))
    Assert.False(Dict.isEmpty (Dict.empty |> addN 1 |> addN 2 |> remN 1))
    let d = seq { for i = 1 to 2000 do yield i } 
            |> Seq.fold (flip addN) Dict.empty
    let has n d = Dict.contains (bs n) d
    let d10 = Dict.dropPrefix (bs 10) d
    Assert.True(has 10 d)
    Assert.False(has 10 d10)
    Assert.True(has 101 d)
    Assert.False(has 101 d10)
    Assert.True(has 1023 d)
    Assert.False(has 1023 d10)
    Assert.True(has 1 d10)
    Assert.True(has 11 d10)
    Assert.Equal(2000 - 111, Seq.length (Dict.toSeq d10)) // 1..2000 - 10,100..109,1000..1099

    let d30 = Dict.selectPrefix (bs 30) d
    Assert.True(has 30 d30)
    Assert.True(has 300 d30)
    Assert.True(has 308 d30)
    Assert.True(has 310 d)
    Assert.False(has 310 d30)
    Assert.True(has 31 d)
    Assert.False(has 31 d30)
    Assert.True(has 200 d)
    Assert.False(has 200 d30)
    Assert.Equal(11, Seq.length (Dict.toSeq d30)) // 30,300,301,302,..309

[<Fact>]
let ``trivial dict parsing`` () =
    let d1 = seq { for i = 1 to 2000 do yield i }
            |> Seq.fold (flip addN) (Dict.empty)
            |> Dict.dropPrefix (bs 10)   // drops 111 items
    let s1 = Dict.writeDict d1
    let mkDef def = Dict.Def(def)
    let mkDir h = raise (System.NotImplementedException()) // no directories!
    let d2 = Dict.parseDict mkDef mkDir s1
    let s2 = Dict.writeDict d2
    Assert.Equal(BS.toString s1, BS.toString s2)
    Assert.Equal(d1,d2)    



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



    // Assuming entries of around 10 bytes each, and pages of 50k, we need
    // ~5000 entries per page. So a few million entries corresponds a few
    // tens of pages, which is sufficient for testing compactions. Or I can
    // try a smaller page/buffer size.
    [<Fact>]
    member tf.``test dict compaction`` () =
        let rng = new System.Random(13)
        let sw_write = new System.Diagnostics.Stopwatch()
        let sw_read = new System.Diagnostics.Stopwatch()
        let cc d = Dict.compact (tf.Stowage) d
        let frac = 1000 
        let compactK k d = if (0 <> (k % frac)) then d else cc d
        let add n d = compactK n (addN n d)
        let rem n d = compactK n (remN n d)
        let a = [| 1 .. 60000 |]
        let r = [| 1 .. (a.Length / 3) |] 
        shuffle rng a
        shuffle rng r

        sw_write.Start()
        let d =  Dict.empty
              |> Array.foldBack add a 
              |> Array.foldBack rem r
              |> Dict.compact (tf.Stowage)
        tf.Flush()
        sw_write.Stop()

        let sz = Dict.sizeDict d
        printfn "dict root node size: %A" sz
        Assert.True((0UL < sz) && (sz < 1600UL))

        let write_op_ct = Array.length a + Array.length r
        let write_op_usec = (1000.0 * sw_write.Elapsed.TotalMilliseconds) / (double write_op_ct)
        printfn "constructed dict containing %A" (Dict.toSeq d |> Seq.map (fst >> BS.toString))
        printfn "final root dict node:\n%s" (d |> Dict.writeDict |> BS.toString)
        printfn "usec per write op: %A" write_op_usec

        Assert.False(Dict.contains (bs 1) d)
        Assert.True(Dict.contains (bs a.Length) d)

        // really doing iteration (sequential read). I might need to test
        // performance of random reads, seperately. But this isn't primarily
        // a performance test.
        let accum s n = s + uint64 n

        sw_read.Start()
        printfn "computing length"
        let len = Dict.toSeq d |> Seq.length
        printfn "len = %A; computing sum" len
        let sum = Dict.toSeq d
                |> Seq.map (fst >> readNat)
                |> Seq.fold accum 0UL
        printfn "sum = %A" sum
        sw_read.Stop()
        printfn "sum: %A, len: %A" sum len
        Assert.Equal(len, Array.length a - Array.length r)

        let array_sum arr = Array.fold accum 0UL arr
        let sum_expected = array_sum a - array_sum r
        Assert.Equal(sum, sum_expected)
        let read_op_ct = 2 * len
        let read_op_usec = (1000.0 * sw_read.Elapsed.TotalMilliseconds) / (double read_op_ct)
        printfn "usec per read op: %A" read_op_usec



