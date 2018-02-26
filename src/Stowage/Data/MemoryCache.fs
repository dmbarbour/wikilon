namespace Stowage

open System.Collections.Generic

module MCache =


    /// A Memory Cache is essentially an unreliable Dictionary. This
    /// is unreliable because elements in the table may be eventually 
    /// deleted by a Stowage.Cache manager in the background. This is
    /// useful for resource lookups, when resources can be reloaded
    /// or regenerated if not present.
    ///
    /// Note: Use of a Lazy<'V> types is appropriate in some cases,
    /// to enable use of `tryAdd` without race conditions on load.
    type C<'K,'V when 'K : equality> = 
        val internal M : Cache.Manager
        val internal D : Dictionary<'K,E<'K,'V>>
        new(cm:Cache.Manager, eq:IEqualityComparer<'K>) = 
            { D = new Dictionary<'K,E<'K,'V>>(eq) 
              M = cm
            }
        new() = 
            let cm = Cache.defaultManager
            let eq = EqualityComparer<'K>.Default
            new C<'K,'V>(cm,eq)
    // cached element type
    and internal E<'K,'V when 'K : equality> =
        val K : 'K
        val V : 'V
        val C : C<'K,'V>
        val mutable TC : int
        new(k,v,c) = { K = k; V = v; C = c; TC = 0 }
        member e.Touch() = e.TC <- (e.TC + 1)
        interface Cached with
            member e.Usage with get() = e.TC
            member e.Clear() = lock (e.C.D) (fun () ->
                e.C.D.Remove(e.K) |> ignore<bool>)


    /// Attempt to load data from the cache. Thread-safe.
    ///
    /// There is no guarantee the key is present, even if recently
    /// added, due to background management of the cache.
    let tryFind (k:'K) (c:C<'K,'V>) : 'V option = 
        lock (c.D) (fun () ->
            match c.D.TryGetValue(k) with
            | true,e -> e.Touch(); Some (e.V)
            | _ -> None)

    /// Add and return data if key is new, otherwise return existing
    /// data. Atomic. Thread-safe. Consider use of Lazy<'V> type to
    /// delay value-load operations.
    let tryAdd (k:'K) (v:'V) (sz:SizeEst) (c:C<'K,'V>) : 'V =
        lock (c.D) (fun () ->
            match c.D.TryGetValue(k) with
            | true,e -> e.Touch(); e.V
            | _ -> 
                let e = new E<'K,'V>(k,v,c)
                c.D.Add(k,e)
                c.M.Receive (e :> Cached) sz
                e.V)

    /// Add data, replacing existing object in cache. Thread-safe.
    let add (k:'K) (v:'V) (sz:SizeEst) (c:C<'K,'V>) : unit =
        let e = new E<'K,'V>(k,v,c)
        lock (c.D) (fun () -> c.D.Add(k,e))
        c.M.Receive (e :> Cached) sz

    /// Remove specified key data from cache. Thread-safe.
    let remove (k:'K) (c:C<'K,'V>) : unit =
        lock (c.D) (fun () -> 
            c.D.Remove(k) |> ignore<bool>)

    /// Remove all keys from cache. Thread-safe.
    let clear (c:C<'K,'V>) : unit =
        lock (c.D) (fun () -> c.D.Clear())



type MCache<'K,'V when 'K : equality> = MCache.C<'K,'V>


