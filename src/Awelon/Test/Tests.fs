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

let testDefStr n = 
    let s = if (0 = n) then "[zero]" else
            "[" + string (n - 1) + " succ] (nat)"
    BS.fromString s

let inline bs n = BS.fromString (string n)
let inline addN (n:int) (d:Dict) : Dict = Dict.add (bs n) (Dict.Def(testDefStr n)) d
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

    member tf.CompactionTest (alen:int) (frac:int) (rng:System.Random) : unit =
        let sw = new System.Diagnostics.Stopwatch()
        let cc d = Codec.compact (Dict.node_codec) (tf.Stowage) d
        let compactK k d = if (0 <> (k % frac)) then d else cc d
        let add n d = compactK n (addN n d)
        let rem n d = compactK n (remN n d)
        let a = [| 1 .. alen |]
        let r = [| 1 .. (alen / 3) |] 
        shuffle rng a
        shuffle rng r

        printfn "building a test dictionary"
        sw.Restart()
        let dRef =  Dict.empty
                 |> Array.foldBack add a 
                 |> Array.foldBack rem r
                 |> cc
                 |> VRef.stow (Dict.codec) (tf.Stowage)
        tf.Flush()
        sw.Stop()
        printfn "test dictionary built (%A ms)" (sw.Elapsed.TotalMilliseconds)
        let write_op_ct = Array.length a + Array.length r
        let write_op_usec = (1000.0 * sw.Elapsed.TotalMilliseconds) / (double write_op_ct)
        printfn "usec per write op: %A" write_op_usec

        // really doing iteration (sequential read). I might need to test
        // performance of random reads, seperately. But this isn't primarily
        // a performance test.
        let accum s n = s + uint64 n
        let array_sum arr = Array.fold accum 0UL arr
        let sum_expected = array_sum a - array_sum r

        sw.Restart()
        let dR = VRef.load dRef
        let sum = Dict.toSeq dR
                |> Seq.map (fst >> readNat)
                |> Seq.fold accum 0UL
        sw.Stop()
        let read_op_ct = (Array.length a) - (Array.length r)
        let read_op_usec = (1000.0 * sw.Elapsed.TotalMilliseconds) / (double read_op_ct)
        printfn "usec per read op: %A" read_op_usec
        Assert.Equal(sum, sum_expected)

        // random-read performance test
        shuffle rng a
        sw.Restart()
        let dRR = VRef.load dRef
        let fnAccum acc k = acc + if Dict.contains (bs k) dRR then uint64 k else 0UL
        let rsum1 = Array.fold fnAccum 0UL a
        sw.Stop()
        let rread_op_ct = Array.length a
        let rread_op_usec = (1000.0 * sw.Elapsed.TotalMilliseconds) / (double rread_op_ct)
        printfn "usec per random read op: %A" rread_op_usec
        Assert.Equal(rsum1,sum_expected)

    [<Fact>]
    member tf.``test dict compaction`` () =
        let rng = new System.Random(1)

        printfn "size 70k, 10 compactions"
        tf.CompactionTest 70000 7000 rng 
        printfn "size 70k, 100 compactions"
        tf.CompactionTest 70000 700 rng 
        printfn "size 70k, 1 compactions"
        tf.CompactionTest 70000 700000 rng

        // following test takes about 30 seconds on my machine 
        //printfn "size 700k, 100 compactions"
        //tf.CompactionTest 700000 7000 rng 
        
    // TODO: test splitAtKey, etc.        
        




