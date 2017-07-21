module Data.ByteString.Tests

#nowarn "988" // suppress warning for empty program

open System
open System.Runtime.InteropServices
open Xunit
open Data.ByteString


[<Fact>]
let ``empty is length zero`` () = 
    Assert.Equal<int>(0, empty.Length)

[<Fact>]
let ``empty equals empty`` () =
    Assert.Equal<ByteString>(empty, empty)
    Assert.NotEqual<ByteString>(empty, singleton 101uy)

[<Fact>]
let ``isEmpty empty`` () = 
    Assert.True(isEmpty empty)
    Assert.False(isEmpty (singleton 101uy))

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
    Assert.Equal<int>(a.Length, 9) // UTF-8 conversions
    Assert.Equal<string>(testStr, a.ToString())
    Assert.Equal<string>(testStr, toString a)

[<Fact>]
let ``empty slice is empty`` () =
    let foo = (fromString "xyzzy").[3..2]
    Assert.True(isEmpty foo)

[<Fact>]
let ``non-empty slices equality`` () =
    let foo = fromString "xyzxyz"
    Assert.Equal<ByteString>(foo.[0..2], foo.[3..5])
    Assert.NotEqual<ByteString>(foo.[0..1], foo.[2..3])

[<Fact>]
let ``slices share underlying array`` () =
    let foo = fromString "xyzzy"
    Assert.Equal<byte[]>(foo.[0..2].UnsafeArray, foo.[2..3].UnsafeArray)

[<Fact>]
let ``simple cons`` () =
    Assert.Equal<ByteString>(fromString "test", cons (byte 't') (fromString "est"))

[<Fact>]
let ``simple append`` () =
    Assert.Equal<ByteString>(fromString "hello, world",
        append (fromString "hello,") (fromString " world"))

[<Fact>]
let ``simple concat`` () =
    Assert.Equal<ByteString>(fromString "hello, world",
        ["hello"; ","; " "; "world"] |> Seq.map fromString |> concat)

[<Fact>]
let ``empty is smallest`` () =
    Assert.True(empty < unsafeCreateA [| 0uy |])

[<Fact>]
let ``lexicographic order`` () =
    Assert.True(fromString "x" < fromString "xx")
    Assert.True(fromString "xx" < fromString "xy")
    Assert.True(fromString "xy" < fromString "yx")

[<Fact>]
let ``ordering on slices`` () =
    let foo = fromString "xyzxyz"
    Assert.True(foo.[1..2] > foo.[3..4])
    Assert.True(foo.[0..1] < foo.[3..5])

[<Fact>]
let ``simple enumerator`` () =
    let x : ByteString = fromString "test==="
    let mutable sum = 0
    for c in (x.[..3]) do
        sum <- (int c) + sum
    Assert.Equal(448, sum)

[<Fact>]
let ``simple fold`` () =
    let x : ByteString = fromString "===test"
    let accum s c = s + int c
    let sum = fold accum 0 (x.[3..]) 
    Assert.Equal(448, sum)

[<Fact>]
let ``pinned data access`` () =
    let x : ByteString = (fromString "==test==").[2..5]
    withPinnedBytes x (fun p ->
        Assert.Equal(116uy, Marshal.ReadByte(p,0))
        Assert.Equal(101uy, Marshal.ReadByte(p,1))
        Assert.Equal(115uy, Marshal.ReadByte(p,2)))
       
        




