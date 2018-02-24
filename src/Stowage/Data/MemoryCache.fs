namespace Stowage

open System.Collections.Generic

module MCache =

    /// A Memory Cache is essentially an unreliable Dictionary. This
    /// is unreliable because elements in the table may be eventually 
    /// deleted by the Stowage.Cache manager thread in the background.
    /// Every item has an associated size estimate, which is counted
    /// against the global memory-cache quota. MCache objects share a
    /// global quota from Stowage.Cache.resize.
    ///
    /// In any case, you shouldn't add data to this dictionary that
    /// you cannot regenerate as needed! And beware potential cache
    /// thrashing issues if the Stowage.Cache is too small.
    ///
    /// Note: Use of a Lazy<'V> types is appropriate in many cases,
    /// to delay loading of data until after it's visible in cache.
    type C<'K,'V when 'K : equality> = 
        val internal Dict : Dictionary<'K,E<'K,'V>>
        new(eq:IEqualityComparer<'K>) = 
            { Dict = new Dictionary<'K,E<'K,'V>>(eq) }
        new() = { Dict = new Dictionary<'K,E<'K,'V>>() }
    and internal E<'K,'V when 'K : equality> =
        val K : 'K
        val V : 'V
        val C : C<'K,'V>
        val mutable TC : int
        new(k,v,c) = { K = k; V = v; C = c; TC = 0 }
        member e.Touch() = e.TC <- (e.TC + 1)
        interface Cached with
            member e.Usage with get() = e.TC
            member e.Clear() = lock (e.C.Dict) (fun () -> 
                e.C.Dict.Remove(e.K) |> ignore<bool>)

    /// Attempt to load data from the cache. Thread-safe.
    let tryFind (k:'K) (c:C<'K,'V>) : 'V option = 
        lock (c.Dict) (fun () ->
            match c.Dict.TryGetValue(k) with
            | true,e -> e.Touch(); Some (e.V)
            | _ -> None)

    /// Add and return data if key is new, otherwise return existing
    /// data. Thread-safe. Consider use of Lazy<'V> type to avoid
    /// loading data more than once.
    let tryAdd (k:'K) (v:'V) (sz:SizeEst) (c:C<'K,'V>) : 'V =
        lock (c.Dict) (fun () ->
            match c.Dict.TryGetValue(k) with
            | true,e -> e.Touch(); e.V
            | _ -> 
                let e = new E<'K,'V>(k,v,c)
                c.Dict.Add(k,e)
                Cache.receive (e :> Cached) sz true
                e.V)

    /// Add data, replacing existing data in the dictionary.
    let add (k:'K) (v:'V) (sz:SizeEst) (c:C<'K,'V>) : unit =
        let e = new E<'K,'V>(k,v,c)
        lock (c.Dict) (fun () -> c.Dict.Add(k,e))
        Cache.receive (e :> Cached) sz true

type MCache<'K,'V when 'K : equality> = MCache.C<'K,'V>


