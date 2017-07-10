module Data.ByteString.Tests

#nowarn "988" // suppress warning for empty program

open System
open Xunit
open Data.ByteString

[<Fact>]
let ``empty is length zero`` () = 
    Assert.Equal<int>(0, empty.Length)

[<Fact>]
let ``empty equals empty`` () =
    Assert.Equal<ByteString>(empty, empty)

[<Fact>]
let ``isEmpty empty`` () = 
    Assert.True((isEmpty empty))

[<Fact>]
let ``isEmpty emptyString`` () =
    Assert.True(isEmpty (fromString ""))

[<Fact>]
let ``empty string converts to empty bytestring`` () =
    Assert.Equal<ByteString>(empty, fromString "")


[<Fact>]
let ``using FNV-1a hash`` () =
    let arr = unsafeCreateA [| 116uy; 101uy; 115uy; 116uy |] // test
    let h32 = ByteString.Hash32 arr
    let h64 = ByteString.Hash64 arr
    Assert.True((2949673445u = h32))
    Assert.True((18007334074686647077UL = h64))

[<Fact>]
let ``basic structural equality`` () =
    Assert.Equal<ByteString>(fromString "hello", fromString "hello")
    Assert.NotEqual<ByteString>(fromString "hello", fromString "goodbye")


[<Fact>]
let ``basic string conversions`` () =
    let testStr = "→↑←"
    let a = fromString testStr
    Assert.Equal<string>(testStr, a.ToString())
    Assert.Equal<string>(testStr, toString a)

(*
[<Fact>]
let ``empty bytestring converts to empty string`` () =
    Assert.True((e.ToString() = ""))


[<Fact>]
let ``empty slice is empty`` () =
    let foo = s "hello"
    let tst = isEmpty (foo.[3..2])
    Assert.True(tst) 

[<Fact>]
let ``slice equality 1`` () =
    let foo = s "xyzxyz"
    Assert.True((foo.[0..2] = foo.[3..5]))
    Assert.False((foo.[0..1] = foo.[1..2]))

[<Fact>]
let ``structural comparisons`` () =
    Assert.True(ByteString() < s "x")
    Assert.False(s "x" < s "")
    Assert.True(s "xx" < s "xy")
    Assert.True(s "xy" < s "y")
    let foo = s "xyzxyz"
    Assert.True(foo.[0..1] < foo.[1..2])
    Assert.False(foo.[1..2] < foo.[3..5])

[<Fact>]
let ``concatenations`` () =
    ()
*)

