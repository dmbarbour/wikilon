namespace Stowage
open Data.ByteString

/// Abstract Remote Storage for Binaries referenced by RscHash.
///
/// Use of secure hashes is a convenient way to reference binary data.
/// They are immutable and acyclic by construction, cacheable, secure,
/// provider-independent, self-authenticating, implicitly shared, 
/// automatically named, decentralized, uniformly sized, and smaller
/// than many full URLs or file paths.
///
/// The idea for Stowage is to build data structures above binaries,
/// in order to represent larger-than-memory data and distributed,
/// purely functional computations. Our stowage layer doubles as a
/// purely functional variant of "virtual memory", since we can move
/// data we won't soon need into higher-latency storage - an external
/// database or filesystem or network.
///
/// Stowage resources may ultimately be GC'd like normal values, and
/// we can easily use conservative GC for references between binaries. 
/// Stowage is usually one aspect of a durable database that determines
/// a durable "root set" for GC. Unlike most databases, Stowage makes
/// it feasible to model entire databases as first-class values within
/// another database - convenient for modeling versioned systems.
///
type Stowage =

    /// The Stow operation should add a value to the Stowage database
    /// and return its RscHash, such that a subsequent Load can access
    /// the data. Additionally, it must atomically Incref the RscHash
    /// to prevent concurrent GC.
    ///
    /// Stowage systems should support values of up to 64MB. Most items
    /// should be much smaller, at most a few hundred kilobytes. If the
    /// item is too large, an appropriate exception should be raised.
    /// 
    /// NOTE: resources containing sensitive data should include a salt,
    /// e.g. an 80-bit random string for entropy. Otherwise, an attacker 
    /// can construct millions of likely hashes and test whether each is 
    /// present within the system.
    abstract member Stow : ByteString -> RscHash

    /// The Load operation should access data from Stowage. If this
    /// data cannot be located, a MissingRsc exception must be raised.
    ///
    /// There is no access control for Stowage, but the RscHash serves
    /// as a secure bearer token and read capability. Consequently, it 
    /// is important that implementations don't expose the full RscHash
    /// through timing attacks. (Exposing the first half is acceptable.)
    abstract member Load : RscHash -> ByteString

    /// RscHash references to binaries can be understood as a form of
    /// unmanaged resource from perspective of our .Net runtime. But
    /// a simple reference counting interface can guard hashes in .Net
    /// memory from a premature GC. Upon stowage, Incref is implicit.
    /// Usually, decref will be performed by .Net finalizer (see VRef).
    abstract member Decref : RscHash -> unit
    abstract member Incref : RscHash -> unit

/// Exception on Load failure.
exception MissingRsc of Stowage * RscHash 

// TODO: Develop a useful set of Stowage combinators. (Low Priority.)
//  layered, cached, mirrored, distributed hashtables...

module Stowage =

    // when tracking deps, don't want to implicitly hold a huge array
    // of origin data in memory, so we'll trimBytes first.
    let private depcons lst rsc = (BS.trimBytes rsc) :: lst 

    // step through dependencies. for use with Seq.unfold
    let rec private streamStep (db:Stowage) (struct(hist,rs)) =
        match rs with
        | (rsc::mrs) ->
            if Set.contains rsc hist then streamStep db (struct(hist,mrs)) else
            let hist' = Set.add rsc hist
            let data = 
                try Some (db.Load rsc)
                with
                | MissingRsc _ -> None
            let rs' =
                match data with
                | Some bytes -> RscHash.foldHashDeps depcons mrs bytes
                | None -> mrs
            Some((rsc,data),struct(hist',rs'))
        | [] -> None

    /// Stream data for the given roots and transitive dependencies,
    /// without repeating any references. Recognizes references by a
    /// conservative algorithm (cf. RscHash.foldHashDeps). Thus, we
    /// may have false positive references. Missing references will
    /// be indicated by `None` in the data field.
    ///
    /// This function can be useful for import-export with a network
    /// or filesystem, generically (rather than data type specific).
    let streamDeps (db:Stowage) (roots:RscHash list) : seq<RscHash * (ByteString option)> =
        Seq.unfold (streamStep db) (struct(Set.empty,roots))

    /// Stream dependencies, but report only the missing entries. An
    /// entry might be missing due to a false positive in recognition
    /// of resource references, so this should be taken as a list of
    /// suggestions for data to provide.
    let missingDeps (db:Stowage) (roots:RscHash list) : seq<RscHash> =
        let rec step st =
            match streamStep db st with
            | Some((rsc,dataOpt),st') ->
                match dataOpt with
                | Some _ -> step st'
                | None -> Some(rsc,st')
            | None -> None
        Seq.unfold step (struct(Set.empty,roots))

    /// Stream dependencies with silent failure for missing entries.
    let availableDeps (db:Stowage) (roots:RscHash list) : seq<RscHash * ByteString> =
        let rec step st =
            match streamStep db st with
            | Some((rsc,dataOpt),st') -> 
                match dataOpt with
                | Some data -> Some((rsc,data),st')
                | None -> step st'
            | None -> None
        Seq.unfold step (struct(Set.empty,roots))


