namespace Data

// The FSharpX ByteString has errors and inefficiencies, and my issue reports
// have been ignored for over a week, so I've rolled my own. Reluctantly.
// Hopefully, one of these days, the F# ecosystem will mature a bit more for
// such basic data structures. 

module ByteString =

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

        member x.GetEnumerator() : System.Collections.Generic.IEnumerator<byte> =
            if (0 = x.Length) then Seq.empty.GetEnumerator() else
            let a = x.UnsafeArray
            let reset = (x.Offset - 1)
            let limit = (x.Length + reset)
            let ix = ref reset 
            { new System.Collections.Generic.IEnumerator<byte> with
                    member e.Current = a.[!ix]
                interface System.Collections.IEnumerator with
                    member e.Current = box a.[!ix]
                    member e.MoveNext() = 
                        if(!ix = limit) then false else
                        ix := (1 + !ix)
                        true
                    member e.Reset() = ix := reset
                interface System.IDisposable with
                    member e.Dispose() = ()
            }

        interface System.Collections.Generic.IEnumerable<byte> with
            member x.GetEnumerator() = x.GetEnumerator()

        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = 
                x.GetEnumerator() :> System.Collections.IEnumerator

        // String conversion assumes an ASCII or UTF-8 encoding. 
        override x.ToString() : string =
            System.Text.Encoding.UTF8.GetString(x.UnsafeArray, x.Offset, x.Length)

        // other interfaces? 
        

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

    let inline singleton (c : byte) = unsafeCreateA (Array.create 1 c)

    let inline ofSeq (s : seq<byte>) : ByteString = unsafeCreateA (Array.ofSeq s)
    let inline ofList (s : byte list) : ByteString = unsafeCreateA (Array.ofList s)

    let inline toArray (s : ByteString) : byte[] = 
        if isEmpty s then Array.empty else
        Array.sub (s.UnsafeArray) (s.Offset) (s.Length)
    let inline toSeq (s : ByteString) : seq<byte> = s :> seq<byte> // IEnumerable<byte>
    let inline toList (s : ByteString) : byte list = List.ofSeq (toSeq s)

    /// concatenate into one large bytestring
    let concat (xs : seq<ByteString>) : ByteString =
        let mem = new System.IO.MemoryStream(4000)
        for x in xs do
            mem.Write(x.UnsafeArray, x.Offset, x.Length)
        unsafeCreateA (mem.ToArray())

    let cons (b : byte) (s : ByteString) : ByteString =
        let mem = Array.zeroCreate (1 + s.Length)
        do mem.[0] <- b
        do Array.blit s.UnsafeArray s.Offset mem 1 s.Length
        unsafeCreateA mem

    let append (a : ByteString) (b : ByteString) : ByteString =
        if isEmpty a then b else
        if isEmpty b then a else
        let mem = Array.zeroCreate (a.Length + b.Length)
        do Array.blit a.UnsafeArray a.Offset mem        0 a.Length
        do Array.blit b.UnsafeArray b.Offset mem a.Length b.Length
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
    let inline span (f : byte -> bool) (x : ByteString) : (ByteString * ByteString) =
        let limit = (x.Offset + x.Length)
        let rec step ix =
            if ((ix = limit) || not (f (x.UnsafeArray.[ix]))) 
                then ix 
                else step (1 + ix)
        let stop = step x.Offset
        let l = unsafeCreate (x.UnsafeArray) (x.Offset) (stop - x.Offset)
        let r = unsafeCreate (x.UnsafeArray) (stop) (x.Length - l.Length)
        (l,r)
    let inline takeWhile f x = fst (span f x)
    let inline dropWhile f x = snd (span f x) 

    /// As 'span', but working right to left
    let inline spanEnd (f : byte -> bool) (x : ByteString) : (ByteString * ByteString) =
        let rec step ix =
            let ix' = ix - 1 
            if ((ix = x.Offset) || not (f (x.UnsafeArray.[ix'])))
                then ix
                else step ix' 
        let stop = step (x.Offset + x.Length)
        let l = unsafeCreate (x.UnsafeArray) (x.Offset) (stop - x.Offset)
        let r = unsafeCreate (x.UnsafeArray) (stop) (x.Length - l.Length)
        (l,r)
    let inline takeWhileEnd f x = snd (spanEnd f x)
    let inline dropWhileEnd f x = fst (spanEnd f x)
        
    /// conversions for other string encodings
    let inline encodeString (s : string) (e : System.Text.Encoding) = 
        unsafeCreateA (e.GetBytes(s))
    let inline decodeString (x : ByteString) (e : System.Text.Encoding) = 
        e.GetString(x.UnsafeArray, x.Offset, x.Length)

    /// fromString and toString assume UTF-8 encoding
    let inline fromString s = encodeString s System.Text.Encoding.UTF8
    let inline toString s = decodeString s System.Text.Encoding.UTF8

    /// Trim excess bytes from bytestring (copy only if excess bytes).
    /// Trimming bytes can simplify reasoning about memory usage.
    let inline trimBytes (s : ByteString) : ByteString =
        if (s.UnsafeArray.Length = s.Length) then s else 
        unsafeCreateA (toArray s)
    







    
