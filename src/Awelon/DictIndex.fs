namespace Awelon
open Data.ByteString
open Stowage
open Parser
open Dict

// Awelon systems generally require indexed dictionaries.
//
// I can start with two basic indices:
//
//  reverse lookup index:   symbol → client
//  behavior version index: word → version
//
// The reverse lookup index will allow us to quickly find all sites
// where a word is used. This is valuable for a lot of use cases. 
// Besides words, we can also track annotations and parse errors.
//
// It is feasible for users to build on this to manually manage tag
// clouds, search words, categories, etc. within a dictionary. This
// can be achieved by defining associated words (such as tags-foo or 
// foo-tags) with a list of tags. But RLU won't support full-text
// lookup directly.
//
// The behavior version index will track each word together with
// its full transitive dependency tree, i.e. a secure hash for a
// subset of a dictionary that defines a particular word. A premise
// is that most words should have stable definitions, and unstable
// definitions should be near the tree root with very few clients.
// Under this premise, our version index can be incremental. The
// version index can also track dependency depth and discovered
// cycles.
//
// Given a version index, we can easily build other indices, such
// as memoized evaluations. 
//
// In general, it might be useful to track incremental update of
// each index, allowing for background computation and maintenance.
// Some laziness for behavior version indices might also be useful.
// But incremental computation adds more complexity than I want to
// suffer at this time. Alternatively, we might use a lazy or even
// memory-only version index.
//

// DictRLU combines a dictionary with a reverse-lookup index.
module DictRLU =

    /// implicit deps for natural numbers, texts, and binaries.
    let private w_zero : Word = BS.fromString "zero"
    let private w_succ : Word = BS.fromString "succ"
    let private w_null : Word = BS.fromString "null"
    let private w_cons : Word = BS.fromString "cons"

    /// If a word's definition fails to parse, we'll record this
    /// in the reverse-lookup using special symbol `EPARSE`. This
    /// allows for swift discovery of all parse errors in a Dict.
    let wEPARSE : Symbol = BS.fromString "EPARSE"

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
    
    let progDeps (p:Program) : Set<Symbol> =
        Seq.fold addTokDeps (Set.empty) (tokenize p)

    /// Parse a program and compute its immediate dependencies.
    /// A program that fails to parse will have a single dependency
    /// on symbol `EPARSE`, for swift lookup of invalid programs.
    let parseDeps (progStr:ByteString) : Set<Symbol> =
        match parse progStr with
        | ParseOK p -> progDeps p
        | ParseFail _ -> Set.singleton wEPARSE

    let private binaryDeps : Set<Symbol> =
        // assume a non-empty binary reference 
        addTokDeps (Set.empty) (struct(TT.Text,w_null))

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


    // REPRESENTATION OF REVERSE-LOOKUP INDEX?
    //
    // Naively, we could just use a trie or dictionary with keys like
    // `symbol@word`. This would combine all references to `symbol` at
    // one location in the tree, but would also make it difficult to
    // merge indices from different volumes of a dictionary because any
    // word may depend on symbol.
    //
    // A "volume" of a dictionary is essentially a common prefix for
    // the words defined within that volume, e.g. the "b" volume or 
    // the "foo-" volume. A volume corresponds to dictionary packages,
    // and aligns with how dictionaries are indexed.
    // 
    // To better isolate volumes, one option is to interleave symbol
    // and word. `SYMBOL@word` might be represented as `wSoYrMdB.OL`.
    // This complicates lookup for SYMBOL because we must scan several
    // volumes, but it also aligns the reverse-lookup with dictionary
    // volumes, which allows for some efficient updates and caching.

    /// A separator '.' is injected between word and symbol within the
    /// mangled key for the reverse lookup index. This must not appear
    /// within the encoded symbols or words. 
    let cSep = byte '.'

    /// This function produces a string representing that the word
    /// directly depends on the symbol. We interleave bytes from 
    /// `word` and `symbol` to `wsoyrmdb.ol`. Logically, we add the
    /// separator `.` to the shorter of symbol and word.
    ///
    /// This results in `symbol` having multiple locations in the 
    /// index aligned with volumes of the dictionary. For example,
    /// all references to `symbol` from words starting with `w` are
    /// in the `ws` index volume. This allows for index updates to
    /// better align with dictionary updates, improving stability
    /// of the index and allows merging a dictionary package index.
    let mangleSymbolAtWord symbol word =
        let lS = BS.length symbol
        let lW = BS.length word
        let mem = Array.zeroCreate (1 + lS + lW)
        let shLen = min lS lW
        for ix = 0 to (shLen - 1) do
            mem.[(2*ix)] <- word.[ix]
            mem.[(2*ix)+1] <- symbol.[ix]
        let off = 2 * shLen
        if (shLen = lW) then 
            // logically add separator to word
            mem.[off] <- cSep
            BS.blit (BS.drop shLen symbol) mem (off+1)
        else 
            // logically add separator to symbol
            mem.[off] <- word.[shLen]
            mem.[off+1] <- cSep
            BS.blit (BS.drop (shLen+1) word) mem (off+2)
        BS.unsafeCreateA mem

    /// The reverse-lookup index is represented by a Dictionary with
    /// mangled keys representing symbols together with locations. 
    /// We can find symbols with some prefix under words with another
    /// prefix, but the lookup is sophisticated due to the mangling.
    /// (See symbolAtWord.)
    /// 
    /// The RLU index does not perform full-text or binary search.
    /// But developers can build on this, introducing manual tags or
    /// categories, e.g. using `foo-tags`, to track topics and other
    /// content for more effective search.
    type RLU = Dict

    module RLU =
        // we'll be leaving the definition fields blank
        let private blank : Def = Inline (BS.empty)

        // empty or initial RLU index
        let empty = Dict.empty

        /// Test if a given word contains a given symbol.
        let isSymbolAtWord symbol word rlu =
            Dict.contains (mangleSymbolAtWord symbol word) rlu

        // active pattern for implicit prefix merge
        let private (|Child|) (struct(p,c)) = Dict.prependPrefix p c


        // To process a reverse-lookup index, we'll frequently need
        // to look at multiple branches, and we want to do this lazily
        // so we'll use the sequence model. So I'll try to cover the
        // common cases.
        //
        // When processing an RLU, there are four major states on two
        // axes to consider based on where we are in a key: initial or
        // final state (before or after the separator), and word or
        // symbol byte. 
        //
        //  IW - initial word byte (before .)
        //  IS - initial symbol byte (before .)
        //  FW - final word byte (after .) 
        //  FS - final symbol byte (after .)
        //
        // These identifiers are used in shorthand.

        let rec private browseSym_IW (s:Symbol) (wp:Prefix) (rlu:RLU) : seq<Word * seq<Word>> =
            let d = Dict.mergeProto rlu // load dictionary
            if Map.isEmpty (d.cs) then Seq.empty else
            let onChild ((ix,(Child c))) =
                if (ix = cSep) 
                    then browseSym_FS s wp c 
                    else browseSym_IS s (BS.snoc wp ix) c
            rlu.cs |> Map.toSeq |> Seq.map onChild
        and private browseSym_IS s wp c =
            if BS.isEmpty s then
                // require match on symbol separator
                let rem = Dict.extractPrefix (BS.singleton cSep) c
                if Dict.isEmpty rem then Seq.empty else
                Seq.singleton (wp, Seq.map fst (Dict.toSeq rem))
            else
                // match on next symbol character
                let rlu = Dict.extractPrefix (BS.take 1 s) c
                browseSym_IW (BS.drop 1 s) wp rlu
        and private browseSym_FS s wp c =
            // word is shorter than symbol, look for symbol entry
            if not (Dict.contains s c) then Seq.empty else
            Seq.singleton (wp, Seq.singleton (BS.empty))
                    
        /// Browse for instances of a symbol. 
        ///
        /// This function supports tree-structured browsing. We find
        /// several prefixes, each of which may have many suffixes,
        /// in a manner aligned with the RLU index nodes.
        let browseSymbol (s:Symbol) (rlu:RLU) : seq<Word * seq<Word>> = 
            Seq.delay (fun () -> browseSym_IW s (BS.empty) rlu)

        let private unbrowse ((wp,ss)) = Seq.map (BS.append wp) ss

        /// Find all instances of a symbol as a flat sequence.
        let matchSymbol (s:Symbol) (rlu:RLU) : seq<Word> =
            browseSymbol s rlu |> Seq.map unbrowse |> Seq.concat

        /// Browse for words matching prefix that contain a symbol.
        /// Efficiently isolate the search to a dictionary volume.
        let browseSymbolInVolume (s:Symbol) (wp:Prefix) (rlu:RLU) : seq<Word * seq<Word>> =
            if BS.isEmpty wp then browseSymbol s rlu else
            Seq.delay (fun () ->
                let k = mangleSymbolAtWord s wp
                if ((BS.length s) < (BS.length wp)) then
                    // e.g. look for `sym` in `word` or `wordy`
                    //   - extract full key `wsoyrmd.` or `wsoyrmd.y`
                    //   - anything that remains is a word suffix
                    let rem = Dict.extractPrefix k rlu
                    if Dict.isEmpty rem then Seq.empty else
                    Seq.singleton (wp, Seq.map fst (Dict.toSeq rem))
                else 
                    // e.g. look for `sym` or `symbol` in `foo`
                    //   - compute key `fsoyom.` or `fsoyom.bol`
                    //   - split prefix `fsoyom` and suffix `bol`
                    //   - extract prefix then browse for suffix
                    let ixSep = (2 * (BS.length wp))
                    assert(cSep = k.[ixSep]) // assumption via mangle def
                    let kp = BS.take ixSep k
                    let s' = BS.drop (ixSep + 1) k
                    let rlu' = Dict.extractPrefix kp rlu
                    browseSym_IW s' wp rlu'
            )

        /// Find all instances of a symbol within a volume, flat sequence.
        let matchSymbolInVolume s wp rlu =
            browseSymbolInVolume s wp rlu |> Seq.map unbrowse |> Seq.concat

        // TODO: Multi-symbol lookups (medium priority)
        //
        // I'm very interested in finding words that contain *all* of
        // several symbols. Mostly, this is useful for keyword or tag 
        // searches. I'm uncertain how feasible this would be, however.
        // Can we take advantage of the RLU volume structure?
        //
        // Say we're looking for symbols `symbol` and `example`, and we
        // have words `foobar` and `bar-food`. Under `foobar`, we try to
        // search both `fsoyombbaorl.` and `feoxoabmaprl.e`. Incrementally,
        // if we search `fe` but fail to find `fs`, we cannot provide any
        // result for `f*`. OTOH, if we DO find both `fe` and `fs`, we can
        // accept preliminary result of `f` and continue to search for
        // symbol suffixes `ymbol` and `xample`. 
        //
        // A related issue is that after we accept `f` we have two branches
        // for `oyombbaorl.` and `oxoabmaprl.e`, but these came originally
        // from the `fs` and `fe` branches. So we must somehow join these
        // branches back together, or allow that we'll be processing our
        // symbols on many distinct branches concurrently.
        //
        // There are a few more challenges with respect to the suffix and
        // our symbols having different sizes.
        //
        // It seems that this is feasible, though not at easy to implement.
        // all easy to implement. It will likely be important long-term, 
        // for discovery and search of manually indexed data.

        (*
        let rec private browseSS_IW (ss:Symbol list) (wp:Prefix) (rlu:RLU) : seq<Word * seq<Word>> =
            let d = Dict.mergeProto rlu // load dictionary
            if Map.isEmpty (d.cs) then Seq.empty else
            let onChild ((ix,(Child c))) =
                if (ix = cSep) 
                    then browseSym_FS ss wp c 
                    else browseSym_IS ss (BS.snoc wp ix) c
            rlu.cs |> Map.toSeq |> Seq.map onChild
        and private browseSS_IS ss wp c =
            if BS.isEmpty s then
                // require match on symbol separator
                let rem = Dict.extractPrefix (BS.singleton cSep) c
                if Dict.isEmpty rem then Seq.empty else
                Seq.singleton (wp, Seq.map fst (Dict.toSeq rem))
            else
                // match on next symbol character
                let rlu = Dict.extractPrefix (BS.take 1 s) c
                browseSym_IW (BS.drop 1 s) wp rlu
        and private browseSym_FS s wp c =
            // word is shorter than symbol, look for symbol entry
            if not (Dict.contains s c) then Seq.empty else
            Seq.singleton (wp, Seq.singleton (BS.empty))

        *)

        // TODO: Partial symbol matches (low priority)
        //
        // It's feasible to partially match symbols within the dictionary.
        // It isn't feasible to limit the results so words appear only once,
        // but we could return unique symbol-word pairs. 
        //


        // Adding definitions.
        let private defBlank : Dict.Def = Dict.Inline (BS.empty)
        let private addWordDeps (w:Word) (deps:Set<Symbol>) (rlu0:RLU) : RLU =
            let addDep rlu sym = Dict.add (symbolAtWord sym w) blank d 
            Set.fold addDep rlu0 deps
        let private remWordDeps (w:Word) (deps:Set<Symbol>) (rlu:RLU) : RLU =
            let remDep rlu sym = Dict.remove (symbolAtWord sym w) rlu
            Set.fold remDep rlu0 deps

        let updWordDef (w:Word) (old_def:Def option) (new_def:Def option) (rlu:RLU) =
            // only index valid words
            if not (isValidWord w) then rlu else
            let old_deps = duDeps old_def
            let new_deps = duDeps new_def
            // assuming most edits are minor, update only the difference
            rlu |> remWordDeps w (Set.difference old_deps new_deps) // remove (old - new)
                |> addWordDeps w (Set.difference new_deps old_deps) // add (new - old)

        // Update RLU with a given difference in definitions.
        // Deletion of a word should erase all dependencies.
        let private onDictDiff rlu ((w,vdiff)) =
            let struct(old_def,new_def) =
                match vdiff with
                | InL a -> struct(a,None)
                | InR b -> struct(None,b)
                | InB (a,b) -> struct(a,b)
            updWordDef w old_def new_def rlu

        /// Update the RLU given initial and final dictionaries.
        let updateByDiff (d0:Dict) (df:Dict) (rlu:RLU) : RLU =
            Dict.diff d0 df |> Seq.fold onDictDiff rlu

        /// Update RLU given the initial dictionary and an entry.
        let applyDictEnt (d0:Dict) (upd:DictEnt) (rlu:RLU) : RLU =
            match upd with
            | Define (w, new_def) -> 
                rlu |> updWordDef (Dict.tryFind w d0) new_def
            | Direct (p, dir) ->
                // update by difference local to prefix
                let d0_at_p = Dict.selectPrefix p d0
                let df_at_p = Dict.prependPrefix p (Dict.fromProto dir)
                updateByDiff (d0_at_p) (df_at_p) rlu

        /// As update, but performs compaction incrementally, ensuring
        /// we don't have the full index in memory at once. 
        let compactingUpdateByDiff (db:Stowage) (d0:Dict) (df:Dict) (rlu0:RLU) : RLU =
            let chunkSize = 2000 // for incremental compaction
            let onChunk rlu chunk = Seq.fold onDictDiff rlu chunk |> Dict.compact db
            Dict.diff d0 df 
                |> Seq.chunkBySize chunkSize
                |> Seq.fold onChunk rlu0

        let compactingApplyDictEnt (db:Stowage) (d0:Dict) (upd:DictEnt) (rlu:RLU) : RLU =
            match upd with
            | Define (w, new_def) ->
                rlu |> updWordDef (Dict.tryFind w d0) new_def
                    |> Dict.compact db
            | Direct (p, dir) ->
                // update by difference local to prefix
                let d0_at_p = Dict.selectPrefix p d0
                let df_at_p = Dict.prependPrefix p (Dict.fromProto dir) 
                compactingUpdateByDiff db d0_at_p df_at_p rlu

        // TODO: memo-cached computation of RLU for more efficient import
        // of dictionaries (requires dictionary union function)

        // TODO: consider optimizing initialization of index. 

    /// DictRLU is assumed to be a consistent pair of dictionary and reverse
    /// lookup index. Consistency can be maintained by use of the DictRLU API.
    type DictRLU =
        { dict : Dict
          rlu  : RLU
        }

    let empty : DictRLU = { dict = Dict.empty; rlu = RLU.empty }

    /// withDict can update the entire dictionary, computing the RLU
    /// incrementally based on the difference of dictionaries.
    let withDict (upd:Dict) (d:DictRLU) : DictRLU =
        { dict = upd
          rlu = RLU.update (d.dict) upd (d.rlu)
        }

    /// fromDict is equivalent to `withDict d empty`, but might be
    /// specialized (eventually; it's low priority)
    let fromDict (d:Dict) : DictRLU = withDict d empty

    /// Incrementally add and index a definition.
    let add (w:Word) (def:Def) (d:DictRLU) : DictRLU =
        let def0 = Dict.tryFind w (d.dict)
        { dict = Dict.add w def (d.dict)
          rlu = RLU.updWordDef w def0 def (d.rlu)
        }

    /// Incrementally remove a word's definition.
    let remove (w:Word) (d:DictRLU) : DictRLU =
        let def0 = Dict.tryFind w (d.dict)
        if Option.isNone def0 then d else
        { dict = Dict.remove w (d.dict)
          rlu = RLU.updWordDef w def0 None (d.rlu)
        }

    /// Update the entire dictionary, via dictionary difference.
    let withDict (upd:Dict) (d0:DictRLU) : DictRLU =
        { dict = upd
          rlu = RLU.updateRLU (d0.dict) upd (d0.rlu)
        }

    /// Search dictionary for definition of a word.
    let inline tryFind w d = Dict.tryFind w (d.dict)

    /// Find all clients of a word as a sequence.
    let wordClients w d = RLU.wordClientsWithPrefix w (BS.empty) (d.rlu)

    /// Look for clients of an annotation, given symbol from within
    /// the annotation parentheses, i.e. given `foo`, look for `(foo)`. 
    let annoClients w d = wordClients (wrapParens w) d

    /// Find all words whose definitions failed to parse.
    let parseErrors d = wordClients wEPARSE d


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
        let upd' = Dict.compact db upd //
        { dict = upd' 
          rlu = RLU.compactingUpdate db (d.dict) upd' (d.rlu)
        }
  
    let compactingFromDict (db:Stowage) (d:Dict) : DictRLU =
        compactingWithDict db d empty
    
    /// A DictRLU is a *consistent* pair of Dict and RLU. Well, we 
    /// assume consistency. Maintaining consistency requires use of
    /// the DictRLU update interface, in general.
    ///
    /// Fortunately, a DictRLU can be maintained incrementally.

(*

//
// NOTE: 
//
// I intend to use a Dict to represent the index. This would enable
// embedding and sharing of indices, where appropriate. Also, use of 
// prefix sharing should ameliorate costs for words with many clients.
module DictX =


    /// Unqualified dependencies for a natural number in Awelon.
    let natDeps (n:Parser.NatTok) : QW list =
        let isZero = (1 = BS.length n) && (byte '0' = n.[0])
        if isZero then [word_zero] else [word_zero; word_succ]

    let private wrapAnno (w:Parser.Word) =
        BS.append3 (BS.singleton (byte '(')) w (BS.singleton (byte ')'))

    /// Unqualified dependencies for an annotation. We'll simply use
    /// the parenthetical annotation, in this case. 
    let annoDeps (anno:Parser.Word) : QW list = [wrapAnno anno]

    /// Unqualified dependencies for an embedded text in Awelon.
    let litDeps (txt:Parser.Text) : QW list =
        // empty string is just the null dependency
        if BS.isEmpty txt then [word_null] else
        let baseDeps = [word_null;word_cons;word_zero;word_succ] 
        // consider adding tokens for full-text search!
        baseDeps

    // TODO: Resource dependencies. (How shall we handle deep dependencies?)
    // 


    /// Given a text string and namespace, compute a list of word-like

    module private StepTok =
        type Tok = (struct(NS * Parser.Token))
        type CX = (struct(NS * Parser.Program)) list
        let rec stepCX (cx:CX) : (Tok * CX) option =
            match cx with
            | (struct(ns,p)::cx') -> stepP ns p cx'
            | [] -> None
        and stepP ns p cx =
            match p with
            | (op::p') -> stepOp ns op (struct(ns,p')::cx)
            | [] -> stepCX cx
        and stepOp ns op cx =
            match op with
            | Parser.Block b -> stepP ns b cx
            | Parser.NS (w,op') -> stepOp (childNS ns w) op' cx
            | Parser.Atom tok -> Some(struct(ns,tok),cx)

    let private tokDeps (ns:NS) (tok:Parser.Token) : QW list =
        failwith "not implemented"

    let rec private stepQW (struct(lst,cx)) =
        match lst with
        | (qw::lst') -> Some(qw, struct(lst',cx))
        | [] ->
            match StepTok.stepCX cx with
            | Some (struct(ns,tok),cx') -> 
                let deps = tokDeps ns tok
                stepQW (struct(deps,cx'))
            | None -> None

//
// Basically, this is represented using a meta dictionary with one
// :word.client entry for each client of a word. This allows for 
// `extractPrefix` to find all clients of a word.
//
// To deal with the hierarchical dictionaries, it's sufficient to 
// include the entries at the appropriate hierarchical layer. Like:
//
// - RLU/word.client        word has client in same dictionary
// - dict/RLU/word.client   dict internally has word as client
// - RLU/dict/word.client   dict/word has an external client
//
// To find all clients of foo/bar/baz, we look at three prefixes:
// 
// - foo/bar/RLU/baz.       internal clients of baz
// - foo/RLU/bar/baz.       clients of bar/baz within foo
// - RLU/foo/bar/baz.       clients of foo/bar/baz
//
// For implementations, we could either try to maintain our RLU index
// within our primary Awelon dictionary, or separately as a companion
// dictionary. The former option is convenient for sharing, but also
// introduces risk that there are errors in the index, and hinders
// some extensions of the reverse lookup.
//
// The `RLU/` could be represented as another `.` without ambiguity.
//
// Besides words, we can index annotations like (par), natural numbers,
// text fragments or words within texts, and so on. We'll also track any
// words whose definitions fail to parse. Secure hash resources are also
// tracked as dependencies, but are not locally indexed as clients.
//
// Note: I should table this for now - priority is relatively low. And
// it might be necessary to support lazy indexing.
        
        


    /// Reverse Dependency Lookup Index.
    ///
    /// Represented as a dictionary with special structure. Assume
    /// a scenario such as:
    ///
    ///   :foo bar d/baz
    ///
    /// In this case, we want to find `foo` given a reference to `bar`
    /// or `d/baz`. So we'll add two entries to our reverse lookup:
    ///
    ///   :^bar.foo
    ///   :^d/baz.foo
    ///
    /// The carrot `^` indicates a shared hierarchical namespace for
    /// client and dependency, to improve structure sharing. If we 
    /// have `d/^bar.foo`, that means `d/bar` depends on `d/foo`.
    ///
    /// To keep it simple, this reverse lookup index is focused on
    /// Awelon words.
    /// index for something like full-text search (one that has more
    /// support for multi-dictionary searches). 
    type RLU = Dict

    type DictX =
        { raw : Dict    // word → definition
          rlu : Dict    // symbol → clients
          ver : Dict    // 
        }

*)
