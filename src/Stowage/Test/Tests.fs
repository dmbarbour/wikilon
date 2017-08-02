module Stowage.Tests

#nowarn "988"

open System
open System.IO
open Xunit
open Stowage.Hash
open Data.ByteString

[<Fact>]
let ``hash test`` () =
    let h0 = fromString "test"
    let h1 = hash h0
    let h2 = hash h1
    let h3 = hash h2
    Assert.Equal<string>(toString h1, "HSjFNGRnqHpFFbPhlThmqCbqkmDSHCBlJNnmDPnDtnCpKHqtNgqhRMJG")
    Assert.Equal<string>(toString h2, "BRqMkFknGGncjKTdrTGMjFFHlGlFmmGGNmcFGPSmGbstsLtpdJnhLNKS")
    Assert.Equal<string>(toString h3, "NLsTsGdQrtFLfDtHJcmqDSmMsDRjnMpCFlkqGfLdgSRhFtTsGqhJrfNN")

let clearTestDir path =
    if Directory.Exists(path) 
        then Directory.Delete(path,true)

// a fixture is needed because we cannot
type TestDB =
    val db : Stowage.DB
    new () =
        let path = "testDB"
        let maxSizeMB = 1000
        do clearTestDir path
        { db = openDB path maxSizeMB }
    interface System.IDisposable with
        member this.Dispose() = closeDB this.db

type DBTests =
    val db : Stowage.DB
    new (fixture : TestDB) = { db = fixture.db }
    interface IClassFixture<TestDB>

    [<Fact>]
    member t.``resource put and get`` () = ()

    [<Fact>]
    member t.``read and write keys`` () = ()

    [<Fact>]
    member t.``simple transaction conflict`` () = ()

    [<Fact>]
    member t.``resource stowage hash`` () = ()
    


