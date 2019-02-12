namespace Awelon
open Data.ByteString
open Stowage
open Parser
open Dict

// Awelon systems generally require indexed dictionaries.
//
// I can start with two basic indices:
//
//  reverse lookup index:   word → client words
//  behavior version cache: word → secure hash version
//
// The reverse lookup index will allow us to quickly find all sites
// where a word is used. This can be leveraged for modeling tags and
// categories, tagging words for automatic testing, etc.. 
//
// For convenience, we can track parse errors and annotations in our
// reverse lookup. We won't invalidate these, but they're still of 
// some potential use. 
//
// A behavior version cache will provide a version for a word with
// its transitive dependencies. The intention is to use as a key for
// later cached computations. Importantly, it allows cache sharing
// and centralizes cache invalidation problems.
//
// Thoughts: Is there a good way to index those undefined words that
// are referenced? At the moment, I don't have a good way, except to
// auto-define missing words as `todo-word` or similar when we first
// use them. 

// DictRLU combines a dictionary with a reverse-lookup index.
module DictRLU =

    /// A reverse-lookup index is represented as a Dictionary with
    /// a `word!client` key for clients of every word except for the
    /// four Awelon primitive words (a b c d). Value field not used.
    ///
    /// We can maintain this index incrementally, meaning that the
    /// cost of updating index is roughly proportional to the change
    /// in the indexed dictionary. Since dictionaries also support
    /// reasonably efficient diffs, this can apply even for changes
    /// to prefix-aligned dictionary packages.
    ///
    /// This index has many applications: caching, reverse lookup of
    /// tag words, and renaming a word or prefix. The primary motive 
    /// is to support caching. 
    type RLU = Dict

    // Thoughts:
    //
    // A weakness of RLU as defined is that it cannot support laziness
    // or separate indexing of packages. I don't think this is a huge
    // problem in practice, but it would hinder lazy downloads of the
    // Stowage packages.
    //
    // Perhaps we could mitigate this by explicitly distinguishing the
    // dictionary we're using vs the reference/origin dictionary. Then
    // we could lazily download the origin and index volumes we need.

    // implicit dependencies for natural numbers, texts, binaries.
    let private w_zero : Word = BS.fromString "zero"
    let private w_succ : Word = BS.fromString "succ"
    let private w_null : Word = BS.fromString "null"
    let private w_cons : Word = BS.fromString "cons"

    // excluding Awelon primitive words from tracked dependencies
    let inline private isPrimC (c:byte) : bool = 
        ((byte 'd') >= c) && (c >= (byte 'a'))
    let inline private isPrim (w:Word) : bool = 
        (1 = (BS.length w)) && (isPrimC (BS.unsafeHead w))

    /// special dependency for Awelon parse-errors
    let depEPARSE : Symbol = BS.fromString "EPARSE"

    let private addTokDeps (s:Set<Symbol>) (struct(tt,w) : Token) : Set<Symbol> =
        match tt with
        | TT.Word -> if isPrim w then s else Set.add w s
        | TT.Anno -> s // annotations are not tracked
        | TT.Nat -> // zero, succ
            let bZero = (1 = BS.length w) && (byte '0' = w.[0])
            if bZero then s |> Set.add w_zero else
            s |> Set.add w_zero |> Set.add w_succ
        | TT.Text -> // null, zero, succ, cons
            if BS.isEmpty w then s |> Set.add w_null else
            s |> Set.add w_null |> Set.add w_cons 
              |> Set.add w_zero |> Set.add w_succ
        | _ -> invalidArg "tt" "unrecognized token type"

    let tokDeps (tok:Token) : Set<Symbol> =
        addTokDeps (Set.empty) tok
    
    let progDeps (p:Program) : Set<Symbol> =
        Seq.fold addTokDeps (Set.empty) (tokenize p)

    /// Parse a program and compute its immediate dependencies.
    /// A program that fails to parse has a dependency on `EPARSE` 
    /// to simplify discovery and debugging.
    let parseDeps (progStr:ByteString) : Set<Symbol> =
        match parse progStr with
        | ParseOK p -> progDeps p
        | ParseFail _ -> Set.singleton depEPARSE

    let private binaryDeps : Set<Symbol> = 
        // binary resources are treated as inline texts
        let exampleBinary = BS.fromString "example binary"
        tokDeps (struct(TT.Text,exampleBinary))

    /// Find dependencies of a definition. Will use EPARSE if the
    /// definition fails to parse. 
    let defDeps (def:Def) : Set<Symbol> =
        match def with
        | Inline s -> parseDeps s
        | Remote ref -> parseDeps (VRef.load ref) 
        | Binary _ -> binaryDeps

    let duDeps (du:Def option) : Set<Symbol> =
        match du with
        | Some def -> defDeps def
        | None -> Set.empty // undefined words are not tracked

    module RLU =

        let ver = byte '1'
        let inline fromV1 (rlu:Dict) : RLU = rlu
        let codec = 
            { new Codec<RLU> with
                member __.Write rlu dst =
                    ByteStream.writeByte ver dst
                    Dict.codec.Write rlu dst
                member __.Read db src =
                    let cVer = ByteStream.readByte src
                    if (cVer = byte '1') then
                        Dict.codec.Read db src |> fromV1
                    else raise ByteStream.ReadError // unknown ver
                member __.Compact db rlu = 
                    let struct(rlu',sz) = Dict.codec.Compact db rlu
                    struct(rlu', 1UL + sz) // add ver byte
            }


        /// Our symbol!client separator.
        let cSep = byte '!'
        do assert((Dict.isValidSymbolChar cSep) 
               && (not (Parser.isWordChar cSep)))

        /// Produce the symbol!client key.
        let inline symbolAtWord symbol word = 
            BS.append3 symbol (BS.singleton cSep) word

        /// empty or initial RLU index
        let empty = Dict.empty

        /// Find all instances of a word within a volume of dictionary
        /// defined by a given prefix (e.g. search only in a package). 
        let searchVolume (wp:Prefix) (k:Symbol) (rlu:RLU) : seq<Word> =
            Seq.delay (fun () ->
                rlu |> Dict.extractPrefix (symbolAtWord k wp)
                    |> Dict.prependPrefix wp
                    |> Dict.toSeq
                    |> Seq.map fst)

        /// Find all instances of a word starting from a given word.
        let searchFrom (w0:Word) (k:Symbol) (rlu:RLU) : seq<Word> =
            Seq.delay (fun () ->
                rlu |> Dict.extractPrefix (BS.snoc k cSep)
                    |> Dict.fromKey w0
                    |> Dict.toSeq
                    |> Seq.map fst)

        /// Find all instances of a word, globally.
        let inline search (k:Symbol) (rlu:RLU) : seq<Word> =
            searchVolume (BS.empty) k rlu

        // Multi-Word Search
        //
        // In this case, we'll attempt to find words that include ALL 
        // words in a given non-empty set. This is mostly intended for
        // tag-based filtering. Reasonably efficient.
        //
        // In the future, we might want to extend this with exclusions.
        module private MultiSearch =
            type SP = Dict list          // multi-search path (below `symbol!`)
            type SS = (Prefix * SP) list // pending search stack (for Seq.unfold)

            // Search step designed for use with Seq.unfold. 
            let rec ssStep (ss0:SS) : (Word * SS) option =
                match ss0 with
                | ((wp,sp0)::ssRem) ->
                    if List.isEmpty sp0 then ssStep ssRem else
                    let sp = List.map (Dict.mergeProto) sp0
                    let found = List.forall (fun d -> Option.isSome (d.vu)) sp
                    let onC ix (struct(cpRem,_)) ss =
                        let cp = BS.cons ix cpRem
                        let spc = List.map (Dict.extractPrefix cp) sp
                        let skip = List.exists (Dict.isEmpty) spc
                        if skip then ss else (((BS.append wp cp),spc)::ss)
                    let ss' = Map.foldBack onC ((List.head sp).cs) ssRem
                    if found then Some (wp,ss') else ssStep ss'
                | [] -> None

            // initial search state for a multi-search volume
            let initSearchVolume (wp:Prefix) (symbols:(Symbol list)) (rlu:RLU) : SS =
                let initSym s = rlu |> Dict.extractPrefix (symbolAtWord s wp)
                let sp = List.map initSym symbols
                let skip = List.exists (Dict.isEmpty) sp
                if skip then [] else ((wp,sp)::[])

            // initial search state for a multi-search with lexicographic
            // starting word (inclusive, but not limited to that prefix).
            let initSearchFrom (w0:Word) (symbols:(Symbol list)) (rlu:RLU) : SS =
                let initSym s = rlu |> Dict.extractPrefix (BS.snoc s cSep)
                                    |> Dict.fromKey w0
                let sp = List.map initSym symbols
                let skip = List.exists (Dict.isEmpty) sp
                if skip then [] else ((BS.empty,sp)::[])

        /// Search for words that contain all given symbols, restricted by
        /// dictionary prefix (e.g. to search a specific package).
        let multiSearchVolume (wp:Prefix) (allOf:Set<Symbol>) (rlu:RLU) : seq<Word> =
            match Set.toList allOf with
            | [] -> invalidArg "allOf" "multi-search requires non-empty set"
            | (k::[]) -> searchVolume wp k rlu
            | symbols -> Seq.delay (fun () -> 
                let ss0 = MultiSearch.initSearchVolume wp symbols rlu
                Seq.unfold (MultiSearch.ssStep) ss0)
        
        /// Search for words that contain all given symbols, starting from
        /// a given word (e.g. to support page-level browsing).
        let multiSearchFrom (w0:Word) (allOf:Set<Symbol>) (rlu:RLU) : seq<Word> =
            match Set.toList allOf with
            | [] -> invalidArg "allOf" "multi-search requires non-empty set"
            | (k::[]) -> searchFrom w0 k rlu
            | symbols -> Seq.delay (fun () ->
                let ss0 = MultiSearch.initSearchFrom w0 symbols rlu
                Seq.unfold (MultiSearch.ssStep) ss0)

        /// Search for words whose definitions use all given symbols.
        let inline multiSearch (allOf:Set<Symbol>) (rlu:RLU) : seq<Word> =
            multiSearchVolume (BS.empty) allOf rlu

        // Constructing the RLU.
        let private blank : Dict.Def = Dict.Inline (BS.empty)
        let private addWordDeps (w:Word) (deps:Set<Symbol>) (rlu0:RLU) : RLU =
            if Set.isEmpty deps then rlu0 else
            let addDep rlu sym = Dict.add (symbolAtWord sym w) blank rlu
            Set.fold addDep rlu0 deps
        let private remWordDeps (w:Word) (deps:Set<Symbol>) (rlu0:RLU) : RLU =
            if Set.isEmpty deps then rlu0 else
            let remDep rlu sym = Dict.remove (symbolAtWord sym w) rlu
            Set.fold remDep rlu0 deps

        let updWordDef (w:Word) (old_def:Def option) (new_def:Def option) (rlu:RLU) =
            // only index valid words
            if not (isValidWord w) then rlu else
            // update the difference in dependencies 
            let old_deps = duDeps old_def
            let new_deps = duDeps new_def
            rlu |> remWordDeps w (Set.difference old_deps new_deps) // remove (old - new)
                |> addWordDeps w (Set.difference new_deps old_deps) // add (new - old)

        // Update RLU with a given difference in definitions.
        // Deletion of a word should erase all dependencies.
        let private onDictDiff rlu ((w,vdiff)) =
            let struct(old_def,new_def) =
                match vdiff with
                | InL a -> struct((Some a),None)
                | InR b -> struct(None,(Some b))
                | InB (a,b) -> struct((Some a),(Some b))
            updWordDef w old_def new_def rlu

        /// Update the RLU given initial and final dictionaries.
        let updateByDiff (d0:Dict) (df:Dict) (rlu:RLU) : RLU =
            Dict.diff d0 df |> Seq.fold onDictDiff rlu

        /// Update RLU given the initial dictionary and an entry.
        let applyDictEnt (d0:Dict) (upd:DictEnt) (rlu:RLU) : RLU =
            match upd with
            | Define (w, new_def) -> 
                let old_def = Dict.tryFind w d0
                rlu |> updWordDef w old_def new_def
            | Direct (p, dir) ->
                // update by difference local to prefix
                let d0_at_p = Dict.selectPrefix p d0
                let df_at_p = Dict.prependPrefix p (Dict.fromProto dir)
                updateByDiff (d0_at_p) (df_at_p) rlu

        /// Update with periodic compaction. 
        let compactingUpdateByDiff (db:Stowage) (d0:Dict) (df:Dict) (rlu0:RLU) : RLU =
            let chunkSize = 2000 // for incremental compaction
            let onChunk rlu chunk = Seq.fold onDictDiff rlu chunk |> Dict.compact db
            Dict.diff d0 df 
                |> Seq.chunkBySize chunkSize
                |> Seq.fold onChunk rlu0

        // TODO: memo-cached computation of RLU for more efficient import
        // of dictionaries (requires efficient dictionary union function)

        // TODO: consider optimizing initialization of index. 

    /// DictRLU is assumed to be a consistent pair of dictionary and reverse
    /// lookup index. Consistency should be maintained by use of the DictRLU
    /// API for updates. Based on the nature of a reverse-lookup index, the 
    /// cost for incremental maintenance is proportional to the size of our
    /// definition updates, so this is easily maintained in real-time.
    [<Struct>]
    type DictRLU =
        { dict : Dict
          rlu  : RLU
        }

    let empty : DictRLU = { dict = Dict.empty; rlu = RLU.empty }

    /// withDict can update the entire dictionary, computing the RLU
    /// incrementally based on the difference of dictionaries.
    let withDict (upd:Dict) (d:DictRLU) : DictRLU =
        { dict = upd
          rlu = RLU.updateByDiff (d.dict) upd (d.rlu)
        }

    /// fromDict is equivalent to `withDict d empty`, but might be
    /// specialized (eventually; it's low priority)
    let fromDict (d:Dict) : DictRLU = withDict d empty

    /// Incrementally add and index a definition.
    let add (w:Word) (def:Def) (d:DictRLU) : DictRLU =
        let def0 = Dict.tryFind w (d.dict)
        { dict = Dict.add w def (d.dict)
          rlu = RLU.updWordDef w def0 (Some def) (d.rlu)
        }

    /// Incrementally remove a word's definition.
    let remove (w:Word) (d:DictRLU) : DictRLU =
        let def0 = Dict.tryFind w (d.dict)
        if Option.isNone def0 then d else
        { dict = Dict.remove w (d.dict)
          rlu = RLU.updWordDef w def0 None (d.rlu)
        }

    /// Update definition for a word.
    let updSym (w:Word) (du:(Def option)) (dict:DictRLU) : DictRLU =
        match du with
        | Some def -> DictRLU.add w def dict
        | None -> DictRLU.remove w dict

    /// Search dictionary for definition of a word.
    let inline tryFind w d = Dict.tryFind w (d.dict)

    /// Find all words that are clients of a given word. 
    let inline search (w:Word) (d:DictRLU) : seq<Word> = 
        RLU.search w (d.rlu)

    /// Find all clients of a word within a given prefix volume.
    let inline searchVolume (inVolume:Prefix) (findThis:Word) (d:DictRLU) : seq<Word> =
        RLU.searchVolume inVolume findThis (d.rlu)

    /// Find clients of a word starting lexicographically from a given word.
    let inline searchFrom (leastResult:Word) (findThis:Word) (d:DictRLU) : seq<Word> =
        RLU.searchFrom leastResult findThis (d.rlu)

    /// Find all parse errors.
    let inline parseErrors (d:DictRLU) : seq<Word> =
        search (depEPARSE) d

    /// Find clients of all symbols under a given volume.
    let multiSearchVolume (inVolume:Prefix) (allOf:Set<Word>) (d:DictRLU) : seq<Word> =
        if Set.isEmpty allOf 
            then d.dict |> Dict.selectPrefix inVolume |> Dict.toSeq |> Seq.map fst
            else RLU.multiSearchVolume inVolume allOf (d.rlu)

    /// Find clients of all symbols starting from least lexicographic result.
    let multiSearchFrom (leastResult:Word) (allOf:Set<Word>) (d:DictRLU) : seq<Word> =
        if Set.isEmpty allOf 
            then d.dict |> Dict.fromKey leastResult |> Dict.toSeq |> Seq.map fst
            else RLU.multiSearchFrom leastResult allOf (d.rlu)

    /// Find clients that use all the given symbols.
    let inline multiSearch (allOf:Set<Word>) (d:DictRLU) : seq<Word> =
        multiSearchVolume (BS.empty) allOf d

    /// Encode DictRLU as a pair of size-prefixed dictionaries.
    let codec = 
        let cP = EncPair.codec' (Dict.codec) (RLU.codec)
        let fromP (struct(dict,rlu)) = { dict = dict; rlu = rlu }
        let toP d = struct(d.dict, d.rlu)
        Codec.view cP fromP toP

    /// Compact the DictRLU. Since it's a pair of dictionaries,
    /// the root size after compaction is at most about 4kB.
    let inline compact (db:Stowage) (d:DictRLU) : DictRLU =
        Codec.compact codec db d

    /// A compacting update to limit and control memory usage.
    /// Suitable for non-incremental differences.
    let compactingWithDict (db:Stowage) (upd:Dict) (d:DictRLU) : DictRLU =
        let upd' = Dict.compact db upd 
        { dict = upd' 
          rlu = RLU.compactingUpdateByDiff db (d.dict) upd' (d.rlu)
        }
  
    let compactingFromDict (db:Stowage) (d:Dict) : DictRLU =
        compactingWithDict db d empty

type RLU = DictRLU.RLU
type DictRLU = DictRLU.DictRLU

// DictVX maintains an indexed version cache above DictRLU.
module DictVX =
    /// A version is a secure hash over a word, its definition, and
    /// transitive dependencies - the smallest dictionary to define
    /// any given word. This serves as key for cached computations.
    /// Conveniently, time-independent versioning enables sharing
    /// cache for similar dictionaries, and we isolate invalidation
    /// to a lazy version cache.
    type V = ByteString

    /// Versions for recursive word definitions are marked by prefix.
    let cCyc = byte 'o'
    do assert(not (RscHash.isHashByte cCyc))

    /// Test for recursive definitions.
    ///
    /// Awelon does not allow recursive definitions, but we may still
    /// cache observations over them (such as projected views), so we 
    /// still need a precise version identifier. For convenience, we
    /// record whether a word definition is recursive in the version.
    let isCyclic (v:V) : bool = 
        (not (BS.isEmpty v)) && (cCyc = (BS.unsafeHead v))

    /// VX is a lazy, indexed version cache.
    ///
    /// We'll compute versions on an as-needed basis, adding them to
    /// the cache. We also may incrementally invalidate versions for
    /// lightly used words via reverse lookup index - this is useful
    /// for the common case. Worst case, we can reset VX to empty.
    ///
    /// Additional Invariants: 
    /// - if a word is in VX, so are all of its dependencies
    /// - all definitions are Inline, simple version strings
    type VX = Dict

    /// Algorithms for versioning and incremental invalidation.
    module VX = 
        let empty : VX = Dict.empty

        /// the version of our versioning function
        let ver = byte '1'
        let inline fromV1 (d:Dict) : VX = 
            if (ver = byte '1') then d else empty

        /// VX codec will write out a VX together with the VX ver value.
        /// On reading, we reset VX if versioning function has changed.
        let codec = 
            { new Codec<VX> with
                member __.Write vx dst =
                    ByteStream.writeByte ver dst
                    Dict.codec.Write vx dst
                member __.Read db src =
                    let cVer = ByteStream.readByte src
                    if (cVer = byte '1') then 
                        Dict.codec.Read db src |> fromV1
                    else raise ByteStream.ReadError // unknown ver
                member __.Compact db vx = 
                    let struct(vx',sz) = Dict.codec.Compact db vx
                    struct(vx', 1UL + sz) // add ver byte
            }

        // using a stack of stateful enumerators during invalidation. It's
        // a little awkward, but avoids too many intermediate structures.
        type private IS = System.Collections.Generic.IEnumerator<Word> list
        let inline private dispose e = (e :> System.IDisposable).Dispose()

        // Invalidate entries from vx, given stack of enumerators and quota.
        let rec private invalidationLoop (rlu:DictRLU.RLU) (vx:VX) (s:IS) (q:int) =
            if ((q < 1) || (List.isEmpty s)) then struct(vx,s,q) else 
            let e = List.head s
            if not (e.MoveNext()) then
                do dispose e
                invalidationLoop rlu vx (List.tail s) q
            else 
                let w = e.Current
                let q' = (q - 1)
                if not (Dict.contains w vx) then
                    invalidationLoop rlu vx s q'
                else 
                    let vx' = Dict.remove w vx
                    let e' = (DictRLU.RLU.search w rlu).GetEnumerator()
                    let s' = (e' :: s)
                    invalidationLoop rlu vx' s' q'

        /// Invalidate a sequence of words from vx, given a valid reverse
        /// lookup index. Effort is limited by quota. If we hit our limit,
        /// we'll return empty VX to rebuild lazily from scratch. Quota 
        /// recommendation is a few thousands.
        let invalidate (quota:int) (rlu:DictRLU.RLU) (ws:seq<Word>) (vx:VX) : VX =
            let s0 = (ws.GetEnumerator()) :: []
            let struct(vx',s',_) = invalidationLoop rlu vx s0 quota
            if List.isEmpty s' then vx' else
            do List.iter dispose s'
            empty

        let vEntry (def:Def) : V =
            match def with
            | Inline v -> v
            | Remote _ | Binary _ -> failwith "invalid version entry"

        // Implementation Notes: regarding recursive definitions
        //
        // Without cyclic definitions, we can simply version each of
        // a word's dependencies then version the word. With cycles,
        // we must generally discover the clique of co-dependent words
        // that must be defined and versioned together.
        //
        // For now, I model this as a triple of word sets:
        //
        //      (clique, deps, queue)
        //
        // Our clique is all the words in our clique. Our dependencies
        // are all the words our clique directly depends upon, including
        // recursive dependencies. The queue includes words that lack a
        // version, which might become part of our clique.
        //
        // In this way, our clique intersected with deps should either be
        // empty (for words that aren't part of a cycle) or equal to the
        // clique (all words are part of the cycle), no in-between states.
        // Alternatively, we could track cyclic structure as a boolean to
        // save a little memory.
        module private Calculate =
            type WS = Set<Word>
            type C = (struct(WS * WS * WS)) // (clique,deps,queue)
            type CS = C list

            // control oversized words for worst-case behavior
            let inline private compactWord (w:Word) : ByteString =
                if (BS.length w < (6 * RscHash.size)) then w else
                BS.cons (byte '@') (RscHash.hash w) 

            // Hash a clique.
            //
            // Current implementation simply writes clique to a bytestring
            // then hashes it. 1MB sufficient for thousands of unique words.
            let hashClique (dict:Dict) (vx:VX) (clique:WS) (deps:WS) : RscHash =
                ByteStream.write (fun dst ->
                    // Write out `word $hash` for clique roots, or `word ~`
                    // if undefined. Always hash definitions for consistency,
                    // independently of dictionary compaction heuristics.
                    clique |> Set.iter (fun w ->
                        ByteStream.writeBytes (compactWord w) dst
                        ByteStream.writeByte (byte ' ') dst
                        match Dict.tryFind w dict with
                        | None -> ByteStream.writeByte (byte '~') dst
                        | Some def ->
                            match def with
                            | Inline v -> 
                                ByteStream.writeByte (byte '$') dst
                                ByteStream.writeBytes (RscHash.hash v) dst
                            | Remote ref ->
                                ByteStream.writeByte (byte '$') dst
                                ByteStream.writeBytes (ref.Addr) dst
                            | Binary ref ->
                                ByteStream.writeByte (byte '%') dst
                                ByteStream.writeBytes (ref.Addr) dst
                        ByteStream.writeByte (byte '\n') dst)

                    // Write out `word !version` for dependencies not in clique.
                    // All direct dependencies are included.
                    (Set.difference deps clique) |> Set.iter (fun w ->
                        ByteStream.writeBytes (compactWord w) dst
                        ByteStream.writeByte (byte ' ') dst
                        match Dict.tryFind w vx with
                        | Some def -> ByteStream.writeBytes (vEntry def) dst
                        | None -> failwith "assumption violated: missing external dep version"
                        ByteStream.writeByte (byte '\n') dst)
                    ) |> RscHash.hash

            // A version is computed from a clique hash and a word. Recursive
            // definitions - where a word is among its own dependencies - will
            // receive a special prefix. 
            //
            // The word version is a little shorter than a RscHash in order to
            // avoid interference with conservative Stowage garbage collection.
            let inline mkV (hC:RscHash) (deps:WS) (w:Word) : V =
                let v0 = (BS.append hC w) |> RscHash.hash |> BS.drop 4
                if not (Set.contains w deps) then v0 else BS.cons cCyc v0 

            // add all words in clique to version index
            let finalizeClique (d:Dict) (vx0:VX) (clique:WS) (deps:WS) : VX =
                let hC = hashClique d vx0 clique deps
                let addCliqueWord vx w = Dict.add w (Inline (mkV hC deps w)) vx
                Set.fold addCliqueWord vx0 clique


            // a union of two cliques
            let inline private unionC (struct(ai,ax,aq):C) (struct(bi,bx,bq):C) =
                let i' = Set.union ai bi
                let x' = Set.union ax bx
                let q' = Set.union aq bq
                struct(i',x',q')

            // Collapse stack of cliques down to cycle index.
            let rec private collapseCycleAtPos ix cs =
                match cs with
                | (a::b::cs') when (ix > 0) -> 
                    collapseCycleAtPos (ix - 1) ((unionC a b)::cs')
                | _ -> cs

            let inline private inClique w (struct(iws,_,_):C) = 
                Set.contains w iws

            // Update the version index
            let rec versionLoopCS (d:Dict) (vx:VX) (cs:CS) : VX =
                match cs with
                | (c::cs') -> versionLoopC d vx cs c
                | [] -> vx // all done!
            and versionLoopC d vx cs (struct(i,x,q):C) =
                if Set.isEmpty q then
                    // word clique fully processed, update vx
                    let vx' = finalizeClique d vx i x
                    versionLoopCS d vx' cs
                else
                    let w = Set.minElement q // choose a word
                    let c' = struct(i,x,Set.remove w q)
                    if Dict.contains w vx
                        then versionLoopC d vx cs c'      // skip versioned word
                        else versionLoopW d vx (c'::cs) w // is unversioned word
            and versionLoopW d vx cs w =
                match List.tryFindIndex (inClique w) cs with
                | None -> // compute version for given word
                    let iw = Set.singleton w // new clique with just one word
                    let xw = DictRLU.duDeps (Dict.tryFind w d)
                    let c = struct(iw,xw,xw) 
                    versionLoopC d vx cs c
                | Some ix -> // cycle detected, collapse cliques 0..ix
                    versionLoopCS d vx (collapseCycleAtPos ix cs)

        /// Update the cache to contain the version for a word and its
        /// transitive dependencies if it does not already, then return
        /// the modified cache and the version.
        let version (d:Dict) (vx0:VX) (w:Word) : struct(VX * V) =
            match Dict.tryFind w vx0 with
            | Some def -> struct(vx0, vEntry def)
            | None ->
                let vx = Calculate.versionLoopW d vx0 (List.empty) w
                match Dict.tryFind w vx with
                | Some def -> struct(vx, vEntry def)
                | None -> failwith "failed to compute version" // should not occur


    /// A DictVX couples a DictRLU and mutable VX.
    ///
    /// The VX is mutable, but should only be mutated by DictVX ops
    /// to maintain consistency with the associated dictionary. 
    /// Entries in the VX must be consistent with the Dict and RLU,
    /// and are logically immutable, so DictVX can be treated as a
    /// value for many use cases. But VX entries won't always be in
    /// memory.

type VX = DictVX.VX
type DictVX = DictVX.DictVX




