namespace Awelon
open Data.ByteString
open Stowage
open Parser
open Dict

// Awelon systems generally require indexed dictionaries.
//
// I can start with two basic indices:
//
//  reverse lookup index:   volume and symbols → words
//  behavior version index: word → secure hash version
//
// The reverse lookup index will allow us to quickly find all sites
// where a word is used. This is valuable for a lot of use cases. 
// Besides dependencies between words, we might track annotations
// and parse errors. We can support multi-symbol lookups so we can
// use words as tags or categories for discovery. We can filter our
// search to a given volume of the dictionary, by prefix or range.
//
// A behavior version index can give us a precise version for any
// specific word and its transitive dependencies, useful for caching
// and memoization tasks and reusable across similar dictionaries.
//
// Given a version index, we can likely build other indices, such
// as memoized evaluations. Alternatively, with a reverse lookup
// index, we can directly maintain evaluated dictionaries, but the
// sharing might be weaker.
//

// DictRLU combines a dictionary with a reverse-lookup index.
module DictRLU =

    /// implicit deps for natural numbers, texts, and binaries.
    let private w_zero : Word = BS.fromString "zero"
    let private w_succ : Word = BS.fromString "succ"
    let private w_null : Word = BS.fromString "null"
    let private w_cons : Word = BS.fromString "cons"

    /// Support fast filter for Awelon parse-errors.
    let depEPARSE : Symbol = BS.fromString "EPARSE"
        // no need to optimize symbol, should be super rare

    // wrap annotation words with parentheses as dependencies.
    let inline private wrapParens (w:Word) : Symbol =
        BS.append3 (BS.singleton (byte '(')) w (BS.singleton (byte ')'))

    let private addTokDeps (s:Set<Symbol>) (struct(tt,w) : Token) : Set<Symbol> =
        match tt with
        | TT.Word -> Set.add w s
        | TT.Anno -> Set.add (wrapParens w) s
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
        | None -> Set.empty

    /// The reverse-lookup index is represented as a Dictionary with
    /// mangled keys representing locations of symbols in another
    /// dictionary. The RLU dictionary value field is not used.
    type RLU = Dict

    module RLU =
        // REPRESENTATION OF THE REVERSE-LOOKUP INDEX
        //
        // Naively, we could just use a trie or dictionary with keys like
        // `symbol!word`. This would join all references to `symbol` at
        // one location in the index. Simple! But inefficient for goals 
        // like local or incremental update of the index, or merging a
        // package index.
        //
        // To ensure locality, entries "near" in the dictionary must also
        // be "near" in the index. Distance in a trie is matching prefix.
        // We can align proximity in index with proximity in the dictionary
        // by interleaving. Instead of `symbol!word` we try `wsoyrmdb!ol`,
        // adding `!` separator to the shorter of word and symbol.
        // 
        // The disadvantage is that this complicates lookups, requiring
        // multi-location search for a symbol. Further, structure sharing
        // and prefix compression will suffer a little. But it's workable.

        /// A separator byte is injected between a word and symbol in the
        /// mangled key for the reverse lookup index. The chosen byte is
        /// is '!' (33). This byte must not appear in our symbols or words.
        let cSep = byte '!'
        do assert((Dict.isValidSymbolChar cSep) && not (Parser.isWordChar cSep))

        // Produce a key representing that a symbol can be found at the
        // definition of word. Interleaves word and symbol bytes, starting
        // with a word byte. Adds cSep to shorter of word or symbol.
        let symbolAtWord symbol word =
            let lS = BS.length symbol
            let lW = BS.length word
            let mem = Array.zeroCreate (1 + lS + lW)
            let shLen = min lS lW
            for ix = 0 to (shLen - 1) do
                let off = (2 * ix)
                mem.[off] <- word.[ix]
                mem.[off+1] <- symbol.[ix]
            let off = 2 * shLen // final offset
            if (shLen = lW) then // logically add separator to word
                mem.[off] <- cSep // terminate word
                BS.blit (BS.drop shLen symbol) mem (off+1) // remaining symbol
            else // logically add separator to symbol
                mem.[off] <- word.[shLen]
                mem.[off+1] <- cSep // terminate symbol
                BS.blit (BS.drop (shLen+1) word) mem (off+2) // remaining word
            BS.unsafeCreateA mem

        /// empty or initial RLU index
        let empty = Dict.empty

        // Implementing multi-symbol search
        //
        // This allows us to find one or more symbols, optionally restricted
        // to a volume of the dictionary, with relative efficiency. Only the
        // results with all symbols are matched. 
        module internal Search =
            type SP = ((Symbol option) * RLU)   // one symbol's search path
            type SS = SP list                   // multi-symbol search path
            type WS = (Prefix * SS) list        // word's search stack

            // test for match on empty word suffix
            let spMatchEmptySuffix ((sOpt,rlu):SP) : bool =
                match sOpt with // match remainder of symbol, if needed
                | None -> Dict.contains (BS.empty) rlu 
                | Some s -> Dict.contains (BS.cons cSep s) rlu

            let spMergeProto ((sOpt,rlu):SP) : SP = 
                (sOpt, Dict.mergeProto rlu)

            // step into symbol search path on a given word-byte
            // filter on symbol byte, too, to recover SP RLU structure
            let spStep (wb:byte) ((sOpt,rlu):SP) : SP =
                assert(cSep <> wb) // cSep is not a valid word byte
                let d = Dict.extractPrefix (BS.singleton wb) rlu
                match sOpt with
                | Some s -> // remove one symbol byte, too.
                    if BS.isEmpty s // if is the final symbol byte?
                      then (None, Dict.extractPrefix (BS.singleton cSep) d)
                      else ((Some (BS.drop 1 s)), Dict.extractPrefix (BS.take 1 s) d) 
                | None -> (None,d) // simple iteration through word bytes

            /// Filter search path to a given word prefix. Only words with
            /// the given prefix will be matched.
            let rec spSkip (wp:Prefix) (sp:SP) : SP =
                if (BS.isEmpty wp) then sp else
                let sp' = spStep (BS.unsafeHead wp) sp
                let wp' = BS.unsafeTail wp
                spSkip wp' sp'

            /// Filter search path lexicographically so we won't return
            /// results earlier than the given initial word. This is for
            /// browsing of search results.
            let spFrom (w0:Word) ((sOpt,rlu):SP) : SP =
                match sOpt with
                | None -> (None, Dict.fromKey w0 rlu)
                | Some s -> (Some s, Dict.fromKey (symbolAtWord s w0) rlu)

            // expand an SS a single step, returning whether the empty
            // suffix is accepted and a filtered child search. This is
            // the primary logic for our multi-symbol search.  
            let ssStep (ss0:SS) : struct(bool * List<struct(byte * SS)>) =
                if List.isEmpty ss0 then struct(false,List.empty) else
                let ss = List.map spMergeProto ss0 // merge up front
                let found = List.forall spMatchEmptySuffix ss // accept wp
                let onC ix _ ws =
                    let ss' = List.map (spStep ix) ss // step all paths
                    let skip = List.exists (snd >> isEmpty) ss' // fail fast 
                    if skip then ws else (struct(ix,ss')::ws)
                // indices of first RLU as initial search filter
                let cs0 = (ss |> List.head |> snd).cs |> Map.remove cSep
                let next = Map.foldBack onC cs0 (List.empty) 
                struct(found,next)

            // Search large step. recursively steps to next output.
            let rec searchStep (ws:WS) : (Word * WS) option =
                match ws with
                | ((wp,ss)::wsRem) ->
                    let struct(found,searchC) = ssStep ss
                    let onC (struct(ixC,ssC)) = ((BS.snoc wp ixC), ssC)
                    let ws' = List.append (List.map onC searchC) wsRem
                    if found then Some (wp,ws') else searchStep ws'
                | [] -> None

            // TODO: consider a horizontal search for browsing, e.g.
            // find several prefixes that are promising for a search.
            // But this is much lower priority.

            /// construct the initial search state:
            ///  - prefix for where to find symbols
            ///  - a collection of symbols to find  
            ///  - a reverse-lookup index to search
            let initSearchVolume (p:Prefix) (symbols:Set<Symbol>) (rlu:RLU) : WS =
                let sToSP s = (Some s, rlu) |> spSkip p 
                let ss0 = symbols |> Set.toList |> List.map sToSP 
                ((p,ss0)::[]) // skips to prefix p

            /// Like initSearchVolume, but lexicographic instead of prefix.
            let initSearchFrom (w:Word) (symbols:Set<Symbol>) (rlu:RLU) : WS =
                let sToSP s = (Some s, rlu) |> spFrom w
                let ss0 = symbols |> Set.toList |> List.map sToSP
                ((BS.empty,ss0)::[]) // no initial prefix

        /// Search a volume of the dictionary for a set of symbols.
        let searchVolume (wp:Prefix) (symbols:Set<Symbol>) (rlu:RLU) : seq<Word> =
            Seq.delay (fun () ->
                let ws0 = Search.initSearchVolume wp symbols rlu
                Seq.unfold (Search.searchStep) ws0)

        /// Search results start at the given word lexicographically.
        /// This is suitable for incremental browsing of search results.
        let searchFrom (w0:Word) (symbols:Set<Symbol>) (rlu:RLU) : seq<Word> =
            Seq.delay (fun () ->
                let ws0 = Search.initSearchFrom w0 symbols rlu
                Seq.unfold (Search.searchStep) ws0)
   
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

    /// Find words whose definitions suffer parse errors.
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


