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

    [< Struct >]
    type Elem =
        val id : nativeint
        val rc : nativeint
        new(id,rc) = { id = id; rc = rc }

    let elemSize = nativeint (2 * IntPtr.Size)
    let idOff = IntPtr.Zero
    let rcOff = nativeint IntPtr.Size
    let inline elemOff (ix:nativeint) : nativeint = (elemSize * ix)

    // would use Marshal.StructureToPtr<T> and converse, but F# is telling me
    // that it isn't a function. Maybe my version of .Net core is off? Anyhow,
    // instead using ReadIntPtr and WriteIntPtr for now.
    let inline getElemAt (p:nativeint) (ix:nativeint) : Elem =
        let src = p + elemOff ix
        let id = Marshal.ReadIntPtr(src + idOff)
        let rc = Marshal.ReadIntPtr(src + rcOff)
        Elem(id,rc)
    let inline setElemAt (p:nativeint) (ix:nativeint) (e:Elem) : unit =
        let dst = p + elemOff ix
        Marshal.WriteIntPtr(dst + idOff, e.id)
        Marshal.WriteIntPtr(dst + rcOff, e.rc)
    let inline clearElemAt p ix = setElemAt p ix (Elem(0n,0n))
        
    // Marshal.AllocHGlobal does not zero-fill memory, so I need to do
    // it explicitly. Fortunately, I don't need great performance here.
    // Allocation is rare, occuring only on init or resize. So I'll just
    // do a simple WriteByte loop.
    let zeroFill (p0:nativeint) (sz:nativeint) : unit =
        let stop = (p0 + sz)
        let rec loop p =
            if (p = stop) then () else
            Marshal.WriteByte(p,0uy)
            loop (1n+p)
        loop p0

    let inline memAllocZF (sz:nativeint) : nativeint =
        let p = Marshal.AllocHGlobal sz
        zeroFill p sz
        p

    // for whatever reason, there is no System.IntPtr.MaxValue.
    let maxNativeInt : nativeint =
        if (System.IntPtr.Size >= 8) then nativeint (System.Int64.MaxValue) else
        let bits = (System.IntPtr.Size * 8) - 1 // exclude sign bit
        nativeint ((1UL <<< bits) - 1UL)

    // Our table is represented in unmanaged memory in order to guarantee
    // it may scale proportional to address space. Geometric growth, powers
    // of two sizes, never empty. At most 2/3 fill.
    // 
    // This table assumes single-threaded access. Locking must be provided
    // by the caller! Also, the table is not reduced in size.
    type Table =
        val mutable private Data : nativeint
        val mutable private Size : nativeint
        val mutable private Fill : nativeint

        new() = 
            let szIni = (1n <<< 12) 
            { Data = memAllocZF (elemOff szIni)
              Size = szIni
              Fill = 0n
            }
        override tbl.Finalize() = 
            tbl.Size <- 0n
            tbl.Fill <- 0n
            Marshal.FreeHGlobal(tbl.Data)
            tbl.Data <- 0n

        // test for finalization for safer shutdown
        member inline private tbl.Finalized() : bool = 
            (0n = tbl.Size)

        // find returns struct(index * refct).
        //
        // This assumes the tbl.Fill < tbl.Size and hence we have some
        // zero elements. If the identifier isn't part of our table, we
        // return the appropriate location (with refct 0)
        member private tbl.Find (id:nativeint) : struct(nativeint * nativeint) =
            let mask = (tbl.Size - 1n)
            let rec loop ix =
                let e = getElemAt (tbl.Data) ix
                if ((e.id = id) || (0n = e.rc)) then struct(ix,e.rc) else
                loop ((1n + ix) &&& mask)
            loop (id &&& mask)

        // get the index, ignore the refct
        member inline private tbl.IndexOf id = 
            let struct(ix,_) = tbl.Find id
            ix
                
        // grow table; geometric growth
        member private tbl.Grow () : unit =
            if (tbl.Finalized())
                then invalidOp "cannot incref after finalization"
            if (tbl.Size > (maxNativeInt / (elemSize * 2n)))
                then raise (System.OutOfMemoryException()) // size will overflow
            assert(tbl.Size > 0n)
            let new_size = (tbl.Size <<< 1)
            let new_data = memAllocZF (elemOff new_size) 
            let old_data = tbl.Data
            let old_size = tbl.Size
            tbl.Data <- new_data
            tbl.Size <- new_size
            let rec loop ix = 
                if (ix = old_size) then () else
                let e = getElemAt old_data ix
                if (0n <> e.rc) 
                    then setElemAt (tbl.Data) (tbl.IndexOf (e.id)) e
                loop (1n+ix)
            loop 0n
            Marshal.FreeHGlobal(old_data)

        member inline private tbl.Reserve() : unit =
            let overfilled = (tbl.Fill * 3n) >= (tbl.Size * 2n)
            if overfilled then tbl.Grow()

        member tbl.Incref (id:nativeint) : unit =
            tbl.Reserve()
            let struct(ix,rc) = tbl.Find id
            if (0n = rc) 
                then tbl.Fill <- (tbl.Fill + 1n)
            setElemAt (tbl.Data) ix (Elem(id, rc + 1n))

        member tbl.Contains (id:nativeint) : bool = 
            let struct(_,rc) = tbl.Find id
            (0n <> rc)

        member tbl.Decref (id:nativeint) : unit =
            if(tbl.Finalized()) then () else 
            let struct(ix,rc) = tbl.Find id
            if (1n = rc) then tbl.DeleteIndex ix else
            if (1n < rc) then setElemAt (tbl.Data) ix (Elem(id, rc - 1n)) else
            invalidOp "refct already zero!"


        // clear the specified index, then walk the hashtable to shift
        // linear collision elements into their appropriate location.
        // This avoids need for sentinel values, but will increase decref
        // costs.
        member private tbl.DeleteIndex (ixDel:nativeint) : unit =
            clearElemAt (tbl.Data) ixDel
            tbl.Fill <- (tbl.Fill - 1n)
            let mask = (tbl.Size - 1n)
            let rec loop ix =
                let e = getElemAt (tbl.Data) ix
                if (0n = e.rc) then () else
                let ix' = tbl.IndexOf (e.id)
                if (ix <> ix')
                    then clearElemAt (tbl.Data) ix
                         setElemAt (tbl.Data) ix' e
                loop ((1n + ix) &&& mask)
            loop ((1n + ixDel) &&& mask)
                

    /// locked incref
    let inline incref (tbl:Table) (id:nativeint) : unit = 
        lock tbl (fun () -> tbl.Incref id)

    /// locked decref
    let inline decref (tbl:Table) (id:nativeint) : unit = 
        lock tbl (fun () -> tbl.Decref id)

    /// locked contains check
    let inline contains (tbl:Table) (id:nativeint) : bool = 
        lock tbl (fun () -> tbl.Contains id)

