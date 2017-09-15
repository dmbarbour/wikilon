module Tests.Data.ByteString

#nowarn "988" // suppress warning for empty program

open System
open System.Runtime.InteropServices
open Xunit
open Data.ByteString


[<Fact>]
let ``empty is length zero`` () = 
    Assert.Equal<int>(0, BS.empty.Length)

[<Fact>]
let ``empty equals empty`` () =
    Assert.Equal<ByteString>(BS.empty, BS.empty)
    Assert.NotEqual<ByteString>(BS.empty, BS.singleton 101uy)

[<Fact>]
let ``isEmpty empty`` () = 
    Assert.True(BS.isEmpty BS.empty)
    Assert.False(BS.isEmpty (BS.singleton 101uy))

[<Fact>]
let ``isEmpty emptyString`` () =
    Assert.True(BS.isEmpty (BS.fromString ""))

[<Fact>]
let ``empty string converts to empty bytestring`` () =
    Assert.Equal<ByteString>(BS.empty, BS.fromString "")


[<Fact>]
let ``using FNV-1a hash`` () =
    let arr = BS.unsafeCreateA [| 116uy; 101uy; 115uy; 116uy |] // test
    let h32 = ByteString.Hash32 arr
    let h64 = ByteString.Hash64 arr
    Assert.True((2949673445u = h32))
    Assert.True((18007334074686647077UL = h64))

[<Fact>]
let ``basic structural equality`` () =
    Assert.Equal<ByteString>(BS.fromString "hello", BS.fromString "hello")
    Assert.NotEqual<ByteString>(BS.fromString "hello", BS.fromString "goodbye")


[<Fact>]
let ``basic string conversions`` () =
    let testStr = "→↑←"
    let a = BS.fromString testStr
    Assert.Equal<int>(a.Length, 9) // UTF-8 conversions
    Assert.Equal<string>(testStr, a.ToString())
    Assert.Equal<string>(testStr, BS.toString a)

[<Fact>]
let ``empty slice is empty`` () =
    let foo = (BS.fromString "xyzzy").[3..2]
    Assert.True(BS.isEmpty foo)

[<Fact>]
let ``non-empty slices equality`` () =
    let foo = BS.fromString "xyzxyz"
    Assert.Equal<ByteString>(foo.[0..2], foo.[3..5])
    Assert.NotEqual<ByteString>(foo.[0..1], foo.[2..3])

[<Fact>]
let ``slices share underlying array`` () =
    let foo = BS.fromString "xyzzy"
    Assert.Equal<byte[]>(foo.[0..2].UnsafeArray, foo.[2..3].UnsafeArray)

[<Fact>]
let ``simple cons`` () =
    Assert.Equal<ByteString>(BS.fromString "test", BS.cons (byte 't') (BS.fromString "est"))

[<Fact>]
let ``simple append`` () =
    Assert.Equal<ByteString>(BS.fromString "hello, world",
        BS.append (BS.fromString "hello,") (BS.fromString " world"))

[<Fact>]
let ``simple concat`` () =
    Assert.Equal<ByteString>(BS.fromString "hello, world",
        ["hello"; ","; " "; "world"] |> Seq.map BS.fromString |> BS.concat)

[<Fact>]
let ``empty is smallest`` () =
    Assert.True(BS.empty < BS.unsafeCreateA [| 0uy |])

[<Fact>]
let ``lexicographic order`` () =
    Assert.True(BS.fromString "x" <  BS.fromString "xx")
    Assert.True(BS.fromString "xx" < BS.fromString "xy")
    Assert.True(BS.fromString "xy" < BS.fromString "yx")

[<Fact>]
let ``ordering on slices`` () =
    let foo = BS.fromString "xyzxyz"
    Assert.True(foo.[1..2] > foo.[3..4])
    Assert.True(foo.[0..1] < foo.[3..5])

[<Fact>]
let ``simple enumerator`` () =
    let x : ByteString = BS.fromString "test==="
    let mutable sum = 0
    for c in (x.[..3]) do
        sum <- (int c) + sum
    Assert.Equal(448, sum)

[<Fact>]
let ``simple fold`` () =
    let x : ByteString = BS.fromString "===test"
    let accum s c = s + int c
    let sum = BS.fold accum 0 (x.[3..]) 
    Assert.Equal(448, sum)

[<Fact>]
let ``pinned data access`` () =
    let x : ByteString = (BS.fromString "==test==").[2..5]
    BS.withPinnedBytes x (fun p ->
        Assert.Equal(116uy, Marshal.ReadByte(p,0))
        Assert.Equal(101uy, Marshal.ReadByte(p,1))
        Assert.Equal(115uy, Marshal.ReadByte(p,2)))
       
[<Fact>]
let ``trivial byte writer`` () =
    let x : ByteString = (BS.fromString "==test==").[2..5]
    let x' = ByteStream.write(fun dst ->
                ByteStream.writeByte x.[0] dst
                ByteStream.writeBytes x.[1..] dst)
    Assert.Equal<ByteString>(x,x')

[<Fact>]
let ``trivial byte reader`` () =
    let x = (BS.fromString "==test==").[2..5]
    let src = new ByteSrc(x)
    let t = ByteStream.readByte src
    let est = ByteStream.readBytes 3 src
    Assert.True(ByteStream.eos src)
    Assert.Equal<byte>(t, byte 't')
    Assert.Equal<ByteString>(est, x.[1..])

[<Fact>] 
let ``test critbit basics`` () =
    let x = BS.unsafeCreateA [| 0xFFuy; 0x00uy; 0xC1uy |]
    Assert.True(BTree.testCritbit 0 x)
    Assert.True(BTree.testCritbit 8 x)
    Assert.True(BTree.testCritbit 9 x)
    Assert.False(BTree.testCritbit 10 x)
    Assert.False(BTree.testCritbit 17 x)
    Assert.True(BTree.testCritbit 18 x)
    Assert.True(BTree.testCritbit 19 x)
    Assert.True(BTree.testCritbit 20 x)
    Assert.False(BTree.testCritbit 21 x)
    Assert.False(BTree.testCritbit 25 x)
    Assert.True(BTree.testCritbit 26 x)
    Assert.False(BTree.testCritbit 27 x)

[<Fact>]
let ``find critbit basics`` () =
    let x = BS.unsafeCreateA [| 0xFFuy; 0x00uy; 0x00uy |]
    let y = BS.unsafeCreateA [| 0xCCuy; 0x00uy |]
    Assert.Equal(Some 3, BTree.findCritbit 0 x y)
    Assert.Equal(Some 3, BTree.findCritbit 1 x y)
    Assert.Equal(Some 3, BTree.findCritbit 2 x y)
    Assert.Equal(Some 3, BTree.findCritbit 3 x y)
    Assert.Equal(Some 4, BTree.findCritbit 4 x y)
    Assert.Equal(Some 7, BTree.findCritbit 5 x y)
    Assert.Equal(Some 7, BTree.findCritbit 7 x y)
    Assert.Equal(Some 8, BTree.findCritbit 8 x y)
    Assert.Equal(Some 18, BTree.findCritbit 9 x y)

[<Fact>]
let ``tree basics`` () =
    let add s t = BTree.add (BS.fromString s) s t
    let d1 : string list = ["bar"; "band"; "bald"; "bandit"; "bald eagle"; "bard"; "barrister"]
    let t1 = List.fold (fun t s -> add s t) BTree.empty d1
    Assert.True(BTree.validate t1)
    Assert.Equal(d1.Length, BTree.size t1)

    Assert.True(BTree.exists (fun k _ -> (byte 'r' = k.[2])) t1)
    Assert.False(BTree.exists (fun k _ -> (byte 'z' = k.[1])) t1)
    Assert.True(BTree.forall (fun k _ -> (byte 'b' = k.[0])) t1)

    //BTree.iter (fun k v -> printfn "%s" v) t1

    let has t s = BTree.containsKey (BS.fromString s) t
    Assert.True(has t1 "band")
    Assert.True(has t1 "bard")
    Assert.True(has t1 "bald")
    Assert.True(has t1 "bar")
    Assert.False(has t1 "test")
    Assert.False(has t1 "")
    Assert.False(has t1 "apple")
    Assert.False(has t1 "barrier")
    Assert.False(has t1 "bardiche")

    Assert.Equal(Some "bard", BTree.tryFind (BS.fromString "bard") t1)
    Assert.Equal(Some "bar", BTree.tryFind (BS.fromString "bar") t1)
    Assert.Equal(Some "bald", BTree.tryFind (BS.fromString "bald") t1)

    let rem s t = BTree.remove (BS.fromString s) t
    let t2 = t1 |> rem "apple" |> rem "bard" |> rem "band" |> rem "bald"
    Assert.True(BTree.validate t2)
    Assert.Equal(d1.Length, 3 + BTree.size t2)
    Assert.False(has t2 "apple")
    Assert.False(has t2 "bard")
    Assert.False(has t2 "band")
    Assert.True(has t2 "bald eagle")
    Assert.True(has t2 "barrister")

    Assert.Equal(t1,t1)
    Assert.Equal(t1, BTree.selectPrefix (BS.fromString "ba") t1)
    Assert.Equal(3, BTree.size (BTree.selectPrefix (BS.fromString "bar") t1))
    Assert.Equal(2, BTree.size (BTree.selectPrefix (BS.fromString "ban") t1))

    let p t s = BTree.partitionK (BS.fromString s) t
    Assert.Equal(t1, snd (p t1 "bald")) // "bald" is least key
    Assert.Equal(2, BTree.size (fst (p t1 "ban"))) // bald and bald eagle to left 
    
    let d1' = List.map snd (BTree.toList t1)
    Assert.Equal<String list>(d1', List.sort d1)




