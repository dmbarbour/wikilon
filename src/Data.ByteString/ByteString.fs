namespace Data.ByteString

open System.Runtime.InteropServices

// Note: since I wrote this, .Net has introduced a standard ReadOnlySpan
// which is very similar - except it assumes external mutability of the
// underlying array, so it still isn't quite what I want.

/// A ByteString represents an immutable slice of a byte array.
[< CustomEquality; CustomComparison; Struct >]
type ByteString =
    val UnsafeArray : byte []
    val Offset : int
    val Length : int
    internal new (arr,off,len) = { UnsafeArray = arr; Offset = off; Length = len }
    // Note: current .Net VM size limit (2017) is about 2GB.

    member inline x.Item (ix : int) : byte = 
        assert ((0 <= ix) && (ix < x.Length)) // debug mode checks
        x.UnsafeArray.[x.Offset + ix]

    member x.GetSlice (iniOpt : int option, finOpt : int option) : ByteString =
        let ini = defaultArg iniOpt 0
        let fin = defaultArg finOpt (x.Length - 1)
        if ((ini < 0) || (fin >= x.Length)) 
            then raise (System.IndexOutOfRangeException "ByteString slice out of range.") 
            else if (fin < ini) then ByteString(Array.empty,0,0) else
            ByteString (x.UnsafeArray, ini + x.Offset, 1 + (fin - ini))

    static member inline FoldLeft f r0 (a:ByteString) =
        let mutable r = r0
        for ix = a.Offset to (a.Offset + a.Length - 1) do
            r <- f r a.UnsafeArray.[ix]
        r

    /// basic FNV-1a hash (32 bits)
    static member Hash32 (a:ByteString) : uint32 =
        let fnv_prime = 16777619u
        let offset_basis = 2166136261u
        let accum h b = ((h ^^^ (uint32 b)) * fnv_prime)
        ByteString.FoldLeft accum offset_basis a

    override x.GetHashCode() = int <| ByteString.Hash32 x
        
    /// basic FNV-1a hash (64 bits)
    static member Hash64 (a:ByteString) : uint64 =
        let fnv_prime = 1099511628211UL
        let offset_basis = 14695981039346656037UL
        let accum h b = ((h ^^^ (uint64 b)) * fnv_prime)
        ByteString.FoldLeft accum offset_basis a

    static member Eq (a:ByteString) (b:ByteString) : bool =
        if (a.Length <> b.Length) then false else
        let rec loop ix =
            if (a.Length = ix) then true else
            if (a.[ix] <> b.[ix]) then false else
            loop (1 + ix)
        loop 0

    override x.Equals (yobj : System.Object) = 
        match yobj with
            | :? ByteString as y -> ByteString.Eq x y
            | _ -> false
    interface System.IEquatable<ByteString> with
        member x.Equals y = ByteString.Eq x y

    /// a constant-time equality comparison on data to control
    /// time-leaks of data. 
    static member CTEq (a:ByteString) (b:ByteString) : bool =
        if (a.Length <> b.Length) then false else
        let rec loop acc ix =
            if (a.Length = ix) then (0uy = acc) else
            let acc' = acc ||| (a.[ix] ^^^ b.[ix])
            loop acc' (ix + 1)
        loop 0uy 0

    static member Compare (a:ByteString) (b:ByteString) : int =
        let sharedLen = min a.Length b.Length
        let rec loop ix =
                if (sharedLen = ix) then 0 else
                let c = compare a.[ix] b.[ix]
                if (0 <> c) then c else
                loop (1 + ix)
        let cmpSharedLen = loop 0 
        if (0 <> cmpSharedLen) then cmpSharedLen else
        compare a.Length b.Length

    interface System.IComparable with
        member x.CompareTo (yobj : System.Object) =
            match yobj with
                | :? ByteString as y -> ByteString.Compare x y
                | _ -> invalidArg "yobj" "cannot compare values of different types"
    interface System.IComparable<ByteString> with
        member x.CompareTo y = ByteString.Compare x y

    member x.ToSeq() = 
        let a = x // copy byref for capture
        seq {
            for ix = (a.Offset) to (a.Offset + a.Length - 1) do
                yield a.UnsafeArray.[ix]
        }
    member inline private x.GetEnumerator() = 
        x.ToSeq().GetEnumerator()

    interface System.Collections.Generic.IEnumerable<byte> with
        member x.GetEnumerator() = x.GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = 
            x.GetEnumerator() :> System.Collections.IEnumerator

    // String conversion assumes an ASCII or UTF-8 encoding. 
    override x.ToString() : string =
        System.Text.Encoding.UTF8.GetString(x.UnsafeArray, x.Offset, x.Length)

    // other interfaces? 

/// The main API for ByteStrings is called 'BS' for shorthand, e.g. `BS.isEmpty`
/// assuming you've aleady opened Data.ByteString. 
module BS =
    let empty : ByteString = ByteString(Array.empty, 0, 0)
    let inline isEmpty (x : ByteString) = (0 = x.Length)
    let inline length (x : ByteString) = x.Length

    /// When creating a bytestring from an array, the client must ensure
    /// the array will not further be mutated. In some contexts, a defensive
    /// copy may be necessary, but that is not automatic.
    let unsafeCreate (arr : byte []) (off : int) (len : int) : ByteString =
        assert ((off >= 0) && (arr.Length >= (off + len)))
        ByteString(arr,off,len)
    let unsafeCreateA (arr : byte []) =
        ByteString(arr, 0, arr.Length)

    let private allBytes = [| System.Byte.MinValue .. System.Byte.MaxValue |]
    do assert(256 = allBytes.Length)

    /// Singleton ByteStrings without runtime allocation
    let singleton (c : byte) = ByteString(allBytes,(int c),1)

    let inline ofSeq (s : seq<byte>) : ByteString = unsafeCreateA (Array.ofSeq s)
    let inline ofList (s : byte list) : ByteString = unsafeCreateA (Array.ofList s)

    let inline toArray (s : ByteString) : byte[] = 
        if isEmpty s then Array.empty else
        Array.sub (s.UnsafeArray) (s.Offset) (s.Length)
    let inline toSeq (s : ByteString) : seq<byte> = s :> seq<byte> // IEnumerable<byte>
    let inline toList (s : ByteString) : byte list = List.ofSeq (toSeq s)

    /// Copy a ByteString (which controls length, etc.) into a byte
    /// array at a given offset.
    let inline blit (src:ByteString) (tgt:byte[]) (tgtOff:int) =
        Array.blit (src.UnsafeArray) (src.Offset) tgt tgtOff (src.Length)

    /// concatenate into one large bytestring
    let concat (xs : seq<ByteString>) : ByteString =
        let mem = new System.IO.MemoryStream()
        for x in xs do
            mem.Write(x.UnsafeArray, x.Offset, x.Length)
        unsafeCreateA (mem.ToArray())

    let cons (b : byte) (s : ByteString) : ByteString =
        let mem = Array.zeroCreate (1 + s.Length)
        do mem.[0] <- b
        do blit s mem 1
        unsafeCreateA mem

    let snoc (s : ByteString) (b : byte) : ByteString =
        let mem = Array.zeroCreate(s.Length + 1)
        do blit s mem 0
        do mem.[s.Length] <- b
        unsafeCreateA mem

    let append (a : ByteString) (b : ByteString) : ByteString =
        if isEmpty a then b else
        if isEmpty b then a else
        let mem = Array.zeroCreate (a.Length + b.Length)
        do blit a mem 0
        do blit b mem (a.Length)
        unsafeCreateA mem

    /// Appending three items comes up quite frequently due to separators.
    let append3 (a:ByteString) (b:ByteString) (c:ByteString) : ByteString =
        let mem = Array.zeroCreate (a.Length + b.Length + c.Length)
        do blit a mem 0
        do blit b mem (a.Length)
        do blit c mem (a.Length + b.Length)
        unsafeCreateA mem

    /// take and drop are slices that won't raise range errors.
    let inline take (n : int) (s : ByteString) : ByteString =
        if (n < 1) then empty else
        if (n >= s.Length) then s else
        unsafeCreate s.UnsafeArray s.Offset n

    let inline drop (n : int) (s : ByteString) : ByteString =
        if (n < 1) then s else
        if (n >= s.Length) then empty else
        unsafeCreate s.UnsafeArray (s.Offset + n) (s.Length - n) 

    /// takeLast and dropLast are like take and drop, but index from the end
    let inline takeLast (n : int) (s : ByteString) : ByteString = drop (s.Length - n) s
    let inline dropLast (n : int) (s : ByteString) : ByteString = take (s.Length - n) s

    /// unsafe accessors, use only when you know the bytestring is non-empty
    let inline unsafeHead (x : ByteString) : byte = 
        x.UnsafeArray.[x.Offset]
    let inline unsafeTail (x : ByteString) : ByteString = 
        unsafeCreate (x.UnsafeArray) (x.Offset + 1) (x.Length - 1)
    
    /// basic left-to-right fold function.
    let inline fold f r0 s = ByteString.FoldLeft f r0 s

    /// head is the first byte, tail is all remaining bytes
    let inline head (x : ByteString) : byte = 
        if isEmpty x then invalidArg "x" "not enough elements" else unsafeHead x
    let inline tail (x : ByteString) : ByteString =
        if isEmpty x then invalidArg "x" "not enough elements" else unsafeTail x

    let inline tryHead (x : ByteString) : byte option =
        if isEmpty x then None else Some (unsafeHead x)
    let inline tryTail (x : ByteString) : ByteString option =
        if isEmpty x then None else Some (unsafeTail x)

    let inline uncons (x : ByteString) : (byte * ByteString) =
        if isEmpty x then invalidArg "x" "not enough elements" else 
        (unsafeHead x, unsafeTail x)
    let inline tryUncons (x : ByteString) : (byte * ByteString) option =
        if isEmpty x then None else Some (unsafeHead x, unsafeTail x)

    /// Split bytestring with longest sequence matched by provided function.
    let inline span (f : byte -> bool) (x : ByteString) : struct(ByteString * ByteString) =
        let limit = (x.Offset + x.Length)
        let rec step ix =
            if ((ix = limit) || not (f (x.UnsafeArray.[ix]))) 
                then ix 
                else step (1 + ix)
        let stop = step x.Offset
        let l = unsafeCreate (x.UnsafeArray) (x.Offset) (stop - x.Offset)
        let r = unsafeCreate (x.UnsafeArray) (stop) (x.Length - l.Length)
        struct(l,r)
    let inline takeWhile f x = 
        let struct(l,_) = span f x
        l
    let inline dropWhile f x = 
        let struct(_,r) = span f x 
        r

    /// Predicate Testing
    let inline forall pred x = isEmpty (dropWhile pred x)
    let inline exists pred x = not (forall (not << pred) x)
    

    /// As 'span', but working right to left
    let inline spanEnd (f : byte -> bool) (x : ByteString) : struct(ByteString * ByteString) =
        let rec step ix =
            let ix' = ix - 1 
            if ((ix = x.Offset) || not (f (x.UnsafeArray.[ix'])))
                then ix
                else step ix' 
        let stop = step (x.Offset + x.Length)
        let l = unsafeCreate (x.UnsafeArray) (x.Offset) (stop - x.Offset)
        let r = unsafeCreate (x.UnsafeArray) (stop) (x.Length - l.Length)
        struct(l,r)
    let inline takeWhileEnd f x = 
        let struct(_,r) = spanEnd f x
        r
    let inline dropWhileEnd f x = 
        let struct(l,_) = spanEnd f x
        l
        
    /// Compute the maximal shared prefix between two strings.
    let sharedPrefix (a:ByteString) (b:ByteString) : ByteString =
        let ixMax = min (a.Length) (b.Length)
        let rec loop ix =
            let halt = (ix = ixMax) || (a.[ix] <> b.[ix])
            if halt then ix else loop (ix + 1)
        take (loop 0) a

    /// Compute the maximal shared suffix between two strings.
    let sharedSuffix (a:ByteString) (b:ByteString) : ByteString =
        let ixMax = min (a.Length) (b.Length)
        let rec loop ix =
            let halt = (ix = ixMax) || (a.[a.Length - ix] <> b.[b.Length - ix])
            if halt then ix else loop (ix + 1)
        takeLast (loop 0) a

    /// conversions for other string encodings
    let inline encodeString (s : string) (e : System.Text.Encoding) = 
        unsafeCreateA (e.GetBytes(s))
    let inline decodeString (x : ByteString) (e : System.Text.Encoding) = 
        e.GetString(x.UnsafeArray, x.Offset, x.Length)

    /// fromString and toString assume UTF-8 encoding
    let inline fromString s = encodeString s System.Text.Encoding.UTF8
    let inline toString s = decodeString s System.Text.Encoding.UTF8

    /// Trim excess bytes from bytestring, if underlying binary is larger
    /// than the bytestring by a given threshold.
    let inline trimBytes' (threshold:int) (s:ByteString) : ByteString =
        if ((s.Length + threshold) >= s.UnsafeArray.Length) then s else
        unsafeCreateA (toArray s) 

    /// Trim excess bytes from bytestring (no threshold)
    let inline trimBytes (s : ByteString) : ByteString = trimBytes' 0 s

    /// Convenient access to pinned bytestring data (for interop).
    let inline withPinnedBytes (s : ByteString) (action : nativeint -> 'x) : 'x =
        let pin = GCHandle.Alloc(s.UnsafeArray, GCHandleType.Pinned)
        try let addr = (nativeint s.Offset) + pin.AddrOfPinnedObject()
            action addr
        finally pin.Free()

    /// Disposable access to Pinned bytestring data.
    ///     use ps = new PinnedByteString(s)
    ///     ... actions with ps.Addr ...
    [<Struct>]
    type PinnedByteString =
        val private Pin : GCHandle
        val BS : ByteString
        val Addr : nativeint
        new(s : ByteString) =
            let pin = GCHandle.Alloc(s.UnsafeArray, GCHandleType.Pinned)
            let addr = (nativeint s.Offset) + pin.AddrOfPinnedObject()
            { Pin = pin; BS = s; Addr = addr }
        member x.Length with get() = x.BS.Length
        interface System.IDisposable with
            member x.Dispose() = x.Pin.Free() 


