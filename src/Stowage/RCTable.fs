
namespace Stowage

open System
open System.Security
open System.Runtime.InteropServices

/// Reference Tracking
///
/// The stowage system tracks secure hash references from .Net memory
/// using a table with reference counts. Resources referenced by this
/// table should not be GC'd from stowage even if they lack persistent
/// roots. The decref will usually be performed via .Net finalizers.
[< SecuritySafeCriticalAttribute >]
module internal RCTable =
    // The current implementation is a hierarchical hashtable with 
    // 64-bit entries where keys use 58 bits and reference counts
    // are 6 bits. 
    //
    // The 'hierarchical' aspect solves the problem of saturating
    // a small 6-bit refct. If we surpass the max refct, we'll add
    // the item to a child table representing a higher 'digit'. We
    // can borrow from this digit upon decref. So a few layers of
    // table essentially give us extra reference count bits.
    //
    // This design is thus best for cases where most items have small
    // reference counts, which is a reasonable assumption for most
    // use cases.
    //
    // Meanwhile, the 58-bit IDs provide reasonable resistance to
    // hash collisions. We'll still likely have a few collisions in
    // a billion references. But for Stowage, we shouldn't have a 
    // billion refs in memory (since that would cost a hundred gigs)
    // and a few collisions isn't going to hurt much.
    [<Struct>]
    type Elem = 
        val v : uint64
        static member rcBits : int = 6
        static member idBits : int = (64 - Elem.rcBits)
        static member rcMask : uint64 = (1UL <<< Elem.rcBits) - 1UL
        static member rcMax  : uint16 = uint16 Elem.rcMask
        static member idMask : uint64 = (1UL <<< Elem.idBits) - 1UL
        new(ev : uint64) = { v = ev }
        new(id : uint64, rc : uint16) = 
            assert(Elem.rcMask >= (uint64 rc))
            Elem((id <<< Elem.rcBits) ||| (uint64 rc))
        member inline e.rc with get() : uint16 = uint16 (e.v &&& Elem.rcMask)
        member inline e.id with get() : uint64 = (e.v >>> Elem.rcBits)
        static member size : int = 8

    let inline elemOff (ix:nativeint) : nativeint = ((nativeint Elem.size) * ix)
    let inline getElemAt (p:nativeint) (ix:nativeint) : Elem =
        // wat? where is ReadUInt64?
        let i64 = Marshal.ReadInt64(p + (elemOff ix))
        Elem(uint64 i64)
    let inline setElemAt (p:nativeint) (ix:nativeint) (e:Elem) =
        Marshal.WriteInt64(p + (elemOff ix), int64 e.v)
    let inline clearElemAt p ix = setElemAt p ix (Elem(0UL))

    let allocData (sz:int) : nativeint =
        assert(sz >= 4)
        let ct = (1n <<< sz)
        let p = Marshal.AllocHGlobal (elemOff ct)
        // initialize the memory
        let rec loop ix = 
            clearElemAt p ix
            if(0n <> ix) then loop (ix - 1n)
        loop (ct - 1n)
        p

    // Our table is represented in unmanaged memory in order to guarantee
    // it may scale proportional to address space. Geometric growth, powers
    // of two sizes, never empty. At most 2/3 fill. The initial size is 
    // a few kilobytes, enough to track a few hundred items.
    // 
    // This table assumes single-threaded access, and should be locked for
    // use from any multi-threaded context.
    [<AllowNullLiteral>]
    type Table =
        val mutable private Data : nativeint
        val mutable private Size : int
        val mutable private Fill : nativeint
        val mutable private Next : Table       // next RC digit or null

        // "digit" size is less than rcMax to provide a small buffer
        // between incref and decref touching the `Next` table.
        static member private Digit : uint16 = 
            let buffer = 7us
            assert(Elem.rcMax > (2us * buffer))
            (Elem.rcMax - buffer)

        private new(sz:int) =
            { Data = allocData sz
              Size = sz
              Fill = 0n
              Next = null
            }
        new() = new Table(10)

        override tbl.Finalize() = 
            Marshal.FreeHGlobal(tbl.Data)
            tbl.Data <- 0n

        // test for finalization for safer shutdown
        member inline private tbl.Finalized() : bool = 
            (0n = tbl.Data)

        // find returns struct(index * refct).
        //
        // This assumes the tbl.Fill < tbl.Size and hence we have some
        // zero elements. If the identifier isn't part of our table, we
        // return the appropriate location (with refct 0)
        member private tbl.Find (idFull:uint64) : struct(nativeint * uint16) =
            let id = (idFull &&& Elem.idMask)
            let mask = ((1n <<< tbl.Size) - 1n)
            let rec loop ix =
                let e = getElemAt (tbl.Data) ix
                if ((e.id = id) || (0us = e.rc)) then struct(ix,e.rc) else
                loop ((1n + ix) &&& mask)
            loop ((nativeint id) &&& mask)

        /// Test whether ID is present within table.
        member tbl.Contains (id:uint64) : bool = 
            if(tbl.Finalized()) then false else
            let struct(_,rc) = tbl.Find id
            (0us <> rc)

        // get the index, ignore the refct
        member inline private tbl.IndexOf id = 
            let struct(ix,_) = tbl.Find id
            ix

        static member private MaxSize : int = 
            (8 * (System.IntPtr.Size - 1))
                
        // grow table; geometric growth
        member private tbl.Grow () : unit =
            if (tbl.Finalized())
                then invalidOp "incref after finalize"
            let old_size = tbl.Size
            let old_data = tbl.Data
            if (old_size >= Table.MaxSize)
                then raise (System.OutOfMemoryException())
            let new_size = (old_size + 1)
            let new_data = allocData new_size
            tbl.Data <- new_data
            tbl.Size <- new_size
            let rec loop ix = 
                let e = getElemAt old_data ix
                if (0us <> e.rc) 
                    then setElemAt (tbl.Data) (tbl.IndexOf (e.id)) e
                if (0n = ix) then () else loop (ix - 1n)
            loop ((1n <<< old_size) - 1n)
            Marshal.FreeHGlobal(old_data)

        member inline private tbl.Reserve() : unit =
            let overfilled = ((tbl.Fill * 3n) >>> (1 + tbl.Size)) <> 0n
            if overfilled then tbl.Grow()

        /// Add ID to the table, or incref existing ID
        member tbl.Incref (id:uint64) : unit =
            tbl.Reserve()
            let struct(ix,rc) = tbl.Find id
            if (0us = rc) then 
                setElemAt (tbl.Data) ix (Elem(id, 1us))
                tbl.Fill <- (tbl.Fill + 1n)
            else if(Elem.rcMax = rc) then
                if (null = tbl.Next) 
                    then tbl.Next <- new Table(7)
                tbl.Next.Incref id
                setElemAt (tbl.Data) ix (Elem(id,(Elem.rcMax - Table.Digit) + 1us))
            else 
                setElemAt (tbl.Data) ix (Elem(id, rc + 1us))

        member tbl.Decref (id:uint64) : unit =
            if(tbl.Finalized()) then () else 
            let struct(ix,rc) = tbl.Find id
            if (1us = rc) then 
                let delete = (null = tbl.Next) || not (tbl.Next.Contains id)
                if delete then tbl.Delete ix else
                // borrow Table.Digit from next table
                tbl.Next.Decref id
                setElemAt (tbl.Data) ix (Elem(id, Table.Digit))
            else if(0us = rc) then invalidOp "refct already zero!"
            else setElemAt (tbl.Data) ix (Elem(id, rc - 1us))

        // clear the specified index, then walk the hashtable to shift
        // potential linear-collision items into appropriate locations
        member private tbl.Delete (ixDel:nativeint) : unit =
            clearElemAt (tbl.Data) ixDel
            tbl.Fill <- (tbl.Fill - 1n)
            let mask = ((1n <<< tbl.Size) - 1n)
            let rec loop ix =
                let e = getElemAt (tbl.Data) ix
                if (0us = e.rc) then () else
                let ix' = tbl.IndexOf (e.id)
                if (ix <> ix')
                    then clearElemAt (tbl.Data) ix
                         setElemAt (tbl.Data) ix' e
                loop ((1n + ix) &&& mask)
            loop ((1n + ixDel) &&& mask)


