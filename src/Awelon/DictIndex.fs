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
// For convenience, we can track parse errors in reverse lookup. I've
// also contemplated tracking annotations, but decided against it at
// this layer.
//
// A behavior version cache will provide a version for a word with
// its transitive dependencies. The intention is to use as a key for
// later cached computations. Importantly, it allows cache sharing
// and centralizes cache invalidation problems.
//

// DictRLU combines a dictionary with a reverse-lookup index.
module DictRLU =

    /// implicit deps for natural numbers, texts, binaries.
    let private w_zero : Word = BS.fromString "zero"
    let private w_succ : Word = BS.fromString "succ"
    let private w_null : Word = BS.fromString "null"
    let private w_cons : Word = BS.fromString "cons"

    /// special dependency for Awelon parse-errors
    let depEPARSE : Symbol = BS.fromString "EPARSE"

    let private addTokDeps (s:Set<Symbol>) (struct(tt,w) : Token) : Set<Symbol> =
        match tt with
        | TT.Word -> Set.add w s
        | TT.Anno -> s // not tracking annotations
        | TT.Nat -> 
            let bZero = (1 = BS.length w) && (byte '0' = w.[0])
            if bZero then s |> Set.add w_zero else
            s |> Set.add w_zero |> Set.add w_succ
        | TT.Text -> 
            if BS.isEmpty w then s |> Set.add w_null else
            s |> Set.add w_null |> Set.add w_cons 
              |> Set.add w_zero |> Set.add w_succ
        | _ -> invalidArg "tt" "unrecognized token type"

    let tokDeps (tok:Token) : Set<Symbol> =
        addTokDeps (Set.empty) tok
    
    let progDeps (p:Program) : Set<Symbol> =
        Seq.fold addTokDeps (Set.empty) (tokenize p)

    /// Parse a program and compute its immediate dependencies.
    /// A program that fails to parse will have a single dependency
    /// on symbol `EPARSE`, for swift lookup of invalid programs.
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

    /// The reverse-lookup index is represented as a Dictionary with
    /// a `word!client` key for every client of every word. The value
    /// field is not used. Definitions with parse errors are recorded
    /// as `EPARSE!word` as the only dependency.
    ///
    /// RLU has many applications - cache invalidation, tags for topic
    /// search or dictionary automation, renaming words, etc.. It is a
    /// very useful index. And it can be maintained incrementally.
    type RLU = Dict

    module RLU =

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
        // words in a given non-empty set. This is mostly useful for
        // tag-based filtering. Reasonably efficient.
        module private MultiSearch =
            type SP = Dict list          // search path (words after `symbol!` extracted)
            type SS = (Prefix * SS) list // search stack (to support Seq.unfold)

            // Expand a search step, and report whether the empty suffix
            // should be included in the output. 
            let spStep (sp0:SP) : struct(bool * List<struct(Prefix * SP)>) =
                if List.isEmpty sp0 then struct(false,List.empty) else
                let sp = sp0 |> List.map (Dict.mergeProto) 
                let found = List.forall (fun d -> Option.isSome (d.vu)) sp
                let onC ix (struct(cp,_)) lst =
                    let p = BS.cons ix cp // maximal prefix
                    let spc = List.map (Dict.extractPrefix p) sp 
                    let skip = List.exists (Dict.isEmpty) spc
                    if skip then lst else (struct(p,spc)::lst)
                let next = Map.foldBack onC ((List.head sp).cs) (List.empty)
                struct(found,next)

            let rec ssStep (ss:SS) : (Word * SS) option =
                match ss with
                | ((wp,sp)::ssRem) ->
                    let struct(found,next) = spStep sp
                    let onC (struct(p,sp')) = ((BS.append wp p),sp')
                    let ss' = List.append (List.map onC next) ssRem
                    if found then Some (wp,ss') else ssStep ss'
                | [] -> None

            // initial search state for a multi-search volume
            let initSearchVolume (wp:Prefix) (symbols:(Symbol list)) (rlu:RLU) : SS =
                let initSym s = rlu |> Dict.extractPrefix (symbolAtWord s wp)
                let sp = List.map initSym symbols
                let skip = List.exists (Dict.isEmpty) sp
                if skip then [] else ((wp,sp)::[])

            // initial search state for a multi-search with starting point
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
            let addDep rlu sym = Dict.add (symbolAtWord sym w) blank rlu
            Set.fold addDep rlu0 deps
        let private remWordDeps (w:Word) (deps:Set<Symbol>) (rlu0:RLU) : RLU =
            let remDep rlu sym = Dict.remove (symbolAtWord sym w) rlu
            Set.fold remDep rlu0 deps

        let updWordDef (w:Word) (old_def:Def option) (new_def:Def option) (rlu:RLU) =
            // only index valid words
            if not (isValidWord w) then rlu else
            // update the precise difference in the dependencies 
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
        RLU.searchFrom leastResult findTHis (d.rlu)

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

    /// Find words under a given prefix that depend on a set of symbols.
    let searchPrefix (p:Prefix) (s:Set<Symbol>) (d:DictRLU) : seq<Word> =
        if Set.isEmpty s // if no symbols, report all words matching prefix
            then d.dict |> Dict.selectPrefix p |> Dict.toSeq |> Seq.map fst
            else RLU.searchVolume p s (d.rlu)

    /// Find words lexicographically from given word that depend on symbols.
    let searchFrom (w:Word) (s:Set<Symbol>) (d:DictRLU) : seq<Word> =
        if Set.isEmpty s // if no symbols, report all words starting from w
            then d.dict |> Dict.fromKey w |> Dict.toSeq |> Seq.map fst
            else RLU.searchFrom w s (d.rlu)

    /// Find multiple symbols globally.
    let search (s:Set<Symbol>) (d:DictRLU) : seq<Word> =
        searchPrefix (BS.empty) s d

    /// Find clients of a single word.
    let inline wordClients w d = search (Set.singleton w) d

    /// Find words whose definitions did not parse.
    let inline parseErrors d = wordClients depEPARSE d

    /// Encode DictRLU as a pair of size-prefixed dictionaries.
    let codec = 
        let cP = EncPair.codec' (Dict.codec) (Dict.codec)
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

// DictVX maintains an indexed version cache above DictRLU.
module DictVX =

    /// A word version is a short, cryptographically unique string
    /// representing the word, its definition, and its transitive
    /// dependencies (for parseable definitions).
    ///
    /// Intended for use as a key mapping to cached computations.
    /// Those other caches can be shared across many dictionaries,
    /// and use simple expiration models instead of invalidation.
    type V = ByteString

    /// version prefix for detected cycles
    let cCyc = byte 'o'
    do assert(not (RscHash.isHashByte cCyc))

    /// Test for cyclic dependencies.
    ///
    /// Calculation of a word version involves cycle detection. We
    /// do still compute a unique version in prsence of cycles, but
    /// cyclic definitions aren't legal in Awelon. For convenience,
    /// we record whether a word is part of a cycle in the version
    /// string. 
    ///
    /// The details of the cycle (which words are involved) are not
    /// tracked and may need to be recomputed.
    let isCyclic (v:V) : bool =
        if BS.isEmpty v 
            then invalidArg "v" "empty string is not a valid version" 
            else (cCyc = (BS.unsafeHead v))

    /// versions with cyclic dependencies. However, cycles are not
    /// legal in Awelon systems, and it's convenient to remem
    /// safely c
    /// computing versions. However, such definitions 
    /// Versions can be safely computed for cyclic dependencies,
    /// but cyclic definitions should not exist in Awelon systems.
    /// For convenience, I


    // Note: We can leverage indirection, mapping from word version
    // to a version for a word's type or link-optimized behavior. 
    // The benefit of doing so would be to further improve sharing,
    // hiding irrelevant structural details.

    /// VX is an indexed word-version cache for a dictionary.
    ///
    /// As a cache, VX is normally incomplete. Missing elements are
    /// computed and added to the cache on an as-needed basis. If a
    /// word has an entry, so must its transitive dependencies (to
    /// simplify invalidation). But empty VX is always valid.
    ///
    /// VX must be invalidated upon update to a word's definition.
    /// Invalidation can be bounded by quota: updates normally erase
    /// a minimal subset of entries, but at quota reset to empty VX.
    type VX = Dict

    // algorithms for invalidation, computing versions
    module VX = 
        let empty : VX = Dict.empty

        /// VX ver represents potential changes to our versioning function.
        let ver : byte = byte '1'

        /// VX codec will write out a VX together with the VX ver value.
        /// On reading, we reset VX if versioning function has changed.
        let codec = 
            let cD = Dict.codec
            { new Codec<VX> with
                member __.Write vx dst =
                    cD.Write vx dst
                    ByteStream.writeByte ver dst
                member __.Read db src =
                    let vx = cD.Read db src
                    let cVer = ByteStream.readByte src
                    if (cVer = ver) then vx else Dict.empty
                member __.Compact db vx = 
                    let struct(vx',sz) = cD.Compact db vx
                    struct(vx', 1UL + sz) // add verVX byte
            }

        // invariant: elements in ws have versions in vx
        let rec private invalidationLoop rlu vx ws q =
            if ((q < 1) || (Set.isEmpty ws)) then struct(vx,ws,q) else
            let w = Set.minElement ws 
            let ws' = DictRLU.RLU.search w rlu 
                    |> Seq.filter (hasVer vx) 
                    |> Seq.fold (fun s c -> Set.add c s) ws 
                    |> Set.remove w 
            let vx' = Dict.remove w vx 
            invalidationLoop rlu vx' ws' (q - 1) // continue!

        /// Invalidation Step.
        ///
        /// Returns final VX and remaining words to invalidate (if any).
        /// This could be used for limited step background invalidation.
        let invalidationStep (quota:int) (rlu:RLU) (ws0:Set<Word>) (vx:VX) : struct(Set<Word> * VX) =
            let ws = Set.filter (hasVer vx) ws0 // for loop invariant
            let struct(vx',ws',_) = invalidationLoop rlu vx ws quota
            struct(ws',vx')

        /// Invalidation of VX. Either completely removes elements in given
        /// set (and all transitive dependencies) or fails and returns the
        /// empty version cache. Max count of removals equals given quota.
        let invalidate (quota:int) (rlu:RLU) (ws:Set<Word>) (vx:VX) : VX =
            let struct(ws',vx') = invalidationStep quota rlu ws vx
            if Set.isEmpty ws' then vx' else empty

        module private Calculate = 
            // Regarding Cyclic Definitions
            //
            // Awelon should not have cyclic definitions, but that's
            // an issue to resolve upon static analysis. Here, I will
            // permit cycles and handle them safely. 
            //
            // To handle cycles, I'll version a "clique" at a time. Each
            // clique will contain a set of interdependent definitions.
            // Then the version of a word can be computed by combining a
            // word with the clique's hash.
            //
            // Within computation, I represent cliques using three sets:
            //
            //   (in clique, dependencies, unknowns)
            //
            // Unknowns are our unprocessed task queue. Dependencies may
            // include words within the clique (in case of cycles). 
We can version our
            // clique when our unknown set is empty. To find cycles, we can
            // check a full computation stack of cliques.
            
            type WS = Set<Word>
            type C = (struct(WS * WS * WS)) // (internal, external, unknown)
            type CS = C list // stack of cliques

            let defV (def:Def) : V =
                match def with
                | Inline s -> s // a V is small, should be inline.
                | _ -> failwith "VX assumption violated: versions are inline"
                    // I could handle these cases, but they shouldn't happen
                    // and I'd prefer an error so I can debug why I'm wrong

            // A version is computed from a clique hash and a word.
            // Essentially append, hash, and drop bytes so it isn't
            // reference-counted in Stowage.
            let inline mkV (hC:RscHash) (w:Word) : V =
                (BS.append h w) |> RscHash.hash |> BS.drop 1

            // To version a clique.
            //
            // The simplest implementation is to simply render the clique
            // as a bytestring then take a secure hash. This might not 
            // scale nicely to super-sized cliques of 100k elements, but
            // those shouldn't be a thing anyway. (TODO: fix when becomes
            // a problem)
            // 
            // Special attention: to avoid dictionary compaction from 
            // affecting versions, we can either inline remote defs or
            // hash the inline defs or load and make a local decision.
            // I choose to hash inline defs for now.
            // 
            // We can update this def, but update VX.ver to reset caches.
            let hashClique (d:Dict) (vx:VX) (clique:WS) (deps:WS) : RscHash =
                ByteStream.write (fun dst ->
                    // write out each `word $hashDef\n`
                    clique |> Set.iter (fun w ->
                        ByteStream.writeBytes w dst
                        ByteStream.writeByte (byte ' ') dst
                        match Dict.tryFind w d with
                        | None -> ByteStream.writeByte (byte '~') dst
                        | Some def ->
                            match def with
                            | Inline s ->
                                ByteStream.writeByte (byte '$') dst
                                ByteStream.writeBytes (RscHash.hash s) dst
                            | Remote ref ->
                                ByteStream.writeByte (byte '$') dst
                                ByteStream.writeBytes (ref.ID) dst
                            | Binary ref ->
                                ByteStream.writeByte (byte '%') dst
                                ByteStream.writeBytes (ref.ID) dst
                        ByteStream.writeByte (byte '\n') dst)
                    // write out each `dep !ver\n`
                    deps |> Set.iter (fun w ->
                        ByteStream.writeBytes w dst
                        ByteStream.writeByte (byte ' ') dst
                        ByteStream.writeByte (byte '!') dst
                        let v = match Dict.tryFind w vx with
                                | Some def -> defV def
                                | None -> failwith "VX invariant violated: deps have versions"
                        ByteStream.writeBytes v dst
                        ByteStream.writeByte (byte '\n') dst)
                ) |> RscHash.hash

            // compute the versions for words in clique, add to VX. 
            let finalizeClique (d:Dict) (vx0:VX) (iws:IWS) (xws:XWS) : VX =
                assert(not (Set.isEmpty iws))
                let hC = hashClique d vx0 iws xws
                let onIW w vx = Dict.add w (Inline (mkV hC w)) vx
                vx0 |> Set.foldBack onIW iws  // add clique words to VX

            let inline private inClique w (struct(iws,_,_):C) = 
                Set.contains w iws
            let inline private mergeC (struct(ai,ax,au):C) (struct(bi,bx,bu):C) =
                let i' = Set.union ai bi
                let x' = Set.union ax bx
                let u' = Set.union au bu
                struct(i',x',u')

            // Collapse stack of cliques based on cycle index.
            // Basically, all intermediate cliques are part of
            // the interdependent cycle.
            let rec private collapseCycle ix cs =
                match cs with
                | (a::b::cs') when (ix > 0) -> 
                    collapseCycle (ix - 1) ((mergeC a b)::cs')
                | _ -> cs

            let rec versionLoop (d:Dict) (vx:VX) (cs:CS) : VX =
                match cs with
                | (c::cs') -> versionLoopC d vx cs c
                | [] -> vx // all done!
            and versionLoopC d vx cs (struct(i,x,u)) =
                if Set.isEmpty u then
                    let vx' = finalizeClique d vx i x
                    versionLoop d vx' cs
                else
                    let w = Set.minElement u // target word
                    let u' = Set.remove w u
                    if Dict.contains w vx then
                        // has version, is external dep
                        let x' = Set.add w x
                        versionLoopC d vx cs (struct(i,x',u'))
                    else // might be external or part of cycle
                        

// if has version, is external dep
                        then versionLoopC d vx cs (struct(i,(Set.add w x),u')) 
                        else versionLoopW d vx (struct(i,x,u')::cs) w
                        
(Set.add w x) u 
                    let u' = Set.remove w u // remove from tasks
                    versionLoopW d vx (struct(i,x,u')::cs') w

            and versionLoopW d vx cs w =
                // don't version any word more than once
                if Dict.contains w vx then versionloop d vx cs else
                // recognize cycle if needed
                let ixCycle = List.tryFindIndex (inClique w) cs
                match ixCycle with
                | None -> 
                    let wi = Set.singleton w // initial clique
                    let wx = Set.empty // initial external deps
                    let wu = 
versionLoop d 
                


            
        open Calculate

        /// Cache and compute a version for the word. Will use version
        /// from cache if possible, otherwise will compute the version
        /// for the word and its transitive dependencies, cache it all,
        /// then return the word's version. Cost is proportional to the
        /// size of all transitive dependencies.
        let version (d:Dict) (vx:VX) (w:Word) : struct(VX * V)
            match Dict.tryFind w vx with
            | Some def -> struct(vx, defV def)
            | None -> 

    /// A DictVX couples a DictRLU and (mutable) VX.
    ///
    /// Entries in the VX must be consistent with the Dict and RLU,
    /// and are logically immutable, so DictVX can be treated as a
    /// value for many use cases. But VX entries won't always be in
    /// memory.





