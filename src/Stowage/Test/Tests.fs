module Stowage.Tests

#nowarn "988"

open System
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



