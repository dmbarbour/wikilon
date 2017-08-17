namespace Stowage
open Data.ByteString

module EncBytes =
    /// Bytes encoding:
    ///  (size)(data)
    ///
    /// (size) uses EncVarNat - i.e. (0..127)*(128..255)
    /// (data) is encoded raw

    let size (b:ByteString) : int =
        EncVarNat.size (uint64 b.Length) + b.Length

    let write (o:System.IO.Stream) (b:ByteString) : unit =
        EncVarNat.write o (uint64 b.Length)
        o.Write(b.UnsafeArray,b.Offset,b.Length)

    let read (i:System.IO.Stream) : ByteString =
        let len = EncVarNat.read i
        if (len > uint64 System.Int32.MaxValue) then failwith "int overflow"
        let arr = Array.zeroCreate (int len)
        let rct = i.Read(arr, 0, arr.Length)
        if (rct <> arr.Length) then failwith "insufficient data"
        Data.ByteString.unsafeCreateA arr


module EncVRef =

    /// Simple VRef Encoding: (bytes)(sep)
    ///
    /// The (sep) byte helps guard recognition of references in
    /// data, and is added if and only if the final byte is a 
    /// valid rscHashByte. The (sep) byte is simply SP (32).
    ///
    /// If it's known that (sep) is unnecessary in context, we
    /// may simply use EncBytes instead of EncVRef. OTOH, the 
    /// overhead for one extra separator character is minor.
    ///
    /// Due to how (bytes) encodes (size), no prefix separator
    /// is required. That is, a Stowage VarNat terminates with
    /// 128..255, which is outside the range for rscHashByte.
    let sep : byte = 32uy

    let sepReq (b : ByteString) : bool =
        (b.Length > 0) && (rscHashByte (b.[b.Length - 1]))

    let size (r:VRef) : int = 
        EncBytes.size r.Bytes
          + (if sepReq r.Bytes then 1 else 0)

    let write (o:System.IO.Stream) (r:VRef) : unit =
        EncBytes.write o (r.Bytes)
        if sepReq (r.Bytes) 
            then o.WriteByte(sep)

    let readSep (i:System.IO.Stream) : unit =
        let b = i.ReadByte()
        if (b <> int sep) then failwith "invalid separator"

    let read (db:DB) (i:System.IO.Stream) : VRef =
        let s = EncBytes.read i
        if sepReq s then readSep i
        increfValDeps db s
        new VRef(db,s)


