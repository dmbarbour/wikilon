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


    




