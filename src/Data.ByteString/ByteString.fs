namespace Data

open System
open System.Collections
open System.Collections.Generic

// The FSharpX ByteString has errors and inefficiencies, and my issue reports
// have been ignored for over a week, so I've rolled my own. Reluctantly.
// Hopefully, one of these days, the F# ecosystem will mature a bit more for
// such basic data structures. 

/// A ByteString represents an immutable slice of a byte array.
///
/// Note: The normal constructors are private to ensure any wrapping
/// of a mutable array uses the searchable word 'unsafe', but you do
/// have full power to provide or access the underlying byte array. 
/// It is left to the client to ensure safe use of ByteStrings.
module ByteString =

    [< CustomEquality; CustomComparison; Struct >]
    type BS private (arr : byte[], off : int, len : int) =
        member x.UnsafeArray = arr
        member x.Offset = off
        member x.Length = len

        private new (arr : byte[]) = BS(arr, 0, arr.Length)

        static member UnsafeCreateA (arr : byte []) = BS arr
        static member UnsafeCreate (arr : byte []) (off : int) (len : int) : BS =
            assert ((off >= 0) && (arr.Length >= (off + len)))
            BS(arr,off,len)

        /// String conversion defaults to UTF-8 in the ByteString
        new (s : string) = BS (System.Text.Encoding.UTF8.GetBytes(s))
        override x.ToString() : string =
            System.Text.Encoding.UTF8.GetString(x.UnsafeArray, x.Offset, x.Length)
        // todo: consider convenience methods for working with streams. 

        member inline x.Item (ix : int) : byte = 
            assert ((0 <= ix) && (ix < x.Length)) // debug mode checks
            x.UnsafeArray.[x.Offset + ix]

        static member inline FoldLeft f r (a:BS) =
            let mutable r = r
            for ix = a.Offset to (a.Offset + a.Length - 1) do
                r <- f r a.UnsafeArray.[ix]
            r

        /// basic FNV-1a hash (32 bits)
        static member Hash32 (a:BS) : uint32 =
            let fnv_prime = 16777619u
            let offset_basis = 2166136261u
            let accum h b = ((h ^^^ (uint32 b)) * fnv_prime)
            BS.FoldLeft accum offset_basis a

        override x.GetHashCode() = int <| BS.Hash32 x
            
        /// basic FNV-1a hash (64 bits)
        static member Hash64 (a:BS) : uint64 =
            let fnv_prime = 1099511628211UL
            let offset_basis = 14695981039346656037UL
            let accum h b = ((h ^^^ (uint64 b)) * fnv_prime)
            BS.FoldLeft accum offset_basis a

        static member Eq (a:BS) (b:BS) : bool =
            if (a.Length <> b.Length) then false else
            if ((a.UnsafeArray = b.UnsafeArray) && (a.Offset = b.Offset)) then true else
            let eof = a.Offset + a.Length
            let rec loop ix =
                if (a.Length = ix) then true else
                if (a.[ix] <> b.[ix]) then false else
                loop (1 + ix)
            loop 0

        override x.Equals (yobj : Object) = 
            match yobj with
                | :? BS as y -> BS.Eq x y
                | _ -> false

        static member Compare (a:BS) (b:BS) : int =
            let compareUpToSharedLen =
                    if ((a.UnsafeArray = b.UnsafeArray) && (a.Offset = b.Offset)) then 0 else
                    let sharedLen = min a.Length b.Length
                    let rec loop ix = 
                        if (sharedLen = ix) then 0 else
                        let c = compare a.[ix] b.[ix]
                        if(0 <> c) then c else
                        loop (1 + ix)
                    loop 0
            if (0 <> compareUpToSharedLen) then compareUpToSharedLen else
            compare a.Length b.Length

        interface System.IComparable with
            member x.CompareTo (yobj : Object) =
                match yobj with
                    | :? BS as y -> BS.Compare x y
                    | _ -> invalidArg "yobj" "cannot compare values of different types"
            
        // more interfaces? 
        //   efficient array slicing
        //   enumerable
        

    /// Create by wrapping a mutable array. It's the client's responsibility
    /// to ensure the array is not mutated after constructing the bytestring.
    let inline unsafeCreateA arr = BS.UnsafeCreateA arr
    let inline unsafeCreate arr off len = BS.UnsafeCreate arr off len


        

    

    
