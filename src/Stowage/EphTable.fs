
namespace Stowage

open System
open System.Security
open System.Runtime.InteropServices

/// Ephemeron Tracking for Stowage
///
/// The stowage system tracks secure hash references from .Net memory
/// using a table with reference counts. Resources referenced by this
/// table should not be GC'd from stowage even if they lack persistent
/// roots. The decref will usually be performed via .Net finalizers.
[< SecuritySafeCriticalAttribute >]
module internal EphTbl =
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
    // a billion references. But I think this is acceptable, it only
    // hurts efficiency of Stowage GC a little bit.


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

    // for whatever reason, there is no System.IntPtr.MaxValue.
    let maxNativeInt : nativeint =
        if (System.IntPtr.Size >= 8) then nativeint (System.Int64.MaxValue) else
        let bits = (System.IntPtr.Size * 8) - 1 // exclude sign bit
        nativeint ((1UL <<< bits) - 1UL)

    let allocData (sz:nativeint) : nativeint =
        assert(sz > 0n)
        if (sz >= (maxNativeInt / (nativeint Elem.size)))
            then raise (System.OutOfMemoryException())
        let p = Marshal.AllocHGlobal (elemOff sz)
        // initialize the memory
        let rec loop ix = 
            clearElemAt p ix
            if(0n <> ix) then loop (ix - 1n)
        loop (sz - 1n)
        p

    let inline freeData p = 
        Marshal.FreeHGlobal(p)


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
        val mutable private Size : nativeint
        val mutable private Fill : nativeint
        val mutable private Next : Table       // next RC digit or null

        // "digit" size is less than rcMax to provide a small buffer
        // between incref and decref touching the `Next` table.
        static member private Digit : uint16 = 
            let buffer = 7us
            assert(Elem.rcMax > (2us * buffer))
            (Elem.rcMax - buffer)

        private new(szIni:nativeint) =
            { Data = allocData szIni
              Size = szIni
              Fill = 0n
              Next = null
            }
        new() = new Table(1n<<<10)

        override tbl.Finalize() = 
            tbl.Size <- 0n
            Marshal.FreeHGlobal(tbl.Data)

        // test for finalization for safer shutdown
        member inline private tbl.Finalized() : bool = 
            (0n = tbl.Size)

        // find returns struct(index * refct).
        //
        // This assumes the tbl.Fill < tbl.Size and hence we have some
        // zero elements. If the identifier isn't part of our table, we
        // return the appropriate location (with refct 0)
        member private tbl.Find (idFull:uint64) : struct(nativeint * uint16) =
            let id = (idFull &&& Elem.idMask)
            let mask = (tbl.Size - 1n)
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
                
        // grow table; geometric growth
        member private tbl.Grow () : unit =
            if (tbl.Finalized())
                then invalidOp "incref after finalize"
            assert(tbl.Size > 0n)
            let new_size = (tbl.Size <<< 1)
            let new_data = allocData new_size
            let old_data = tbl.Data
            let old_size = tbl.Size
            tbl.Data <- new_data
            tbl.Size <- new_size
            let rec loop ix = 
                if (ix = old_size) then () else
                let e = getElemAt old_data ix
                if (0us <> e.rc) 
                    then setElemAt (tbl.Data) (tbl.IndexOf (e.id)) e
                loop (ix + 1n)
            loop 0n
            Marshal.FreeHGlobal(old_data)

        member inline private tbl.Reserve() : unit =
            let overfilled = (tbl.Fill * 3n) >= (tbl.Size * 2n)
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
                    then tbl.Next <- new Table(1n<<<7)
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
            let mask = (tbl.Size - 1n)
            let rec loop ix =
                let e = getElemAt (tbl.Data) ix
                if (0us = e.rc) then () else
                let ix' = tbl.IndexOf (e.id)
                if (ix <> ix')
                    then clearElemAt (tbl.Data) ix
                         setElemAt (tbl.Data) ix' e
                loop ((1n + ix) &&& mask)
            loop ((1n + ixDel) &&& mask)
