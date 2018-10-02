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
    let private wrapParens (w:Word) : Symbol =
        let lP = BS.singleton (byte '(')
        let rP = BS.singleton (byte ')')
        BS.append3 lP w rP

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

    let defDeps (def:Def) : Set<Symbol> =
        match def with
        | Inline s -> parseDeps s
        | Remote ref -> parseDeps (VRef.load ref) 
        | Binary _ -> binaryDeps

    /// Our reverse-lookup index can be represented using a dictionary
    /// for convenient export. We'll only use the keys from the trie.
    /// Each key of form `word@client` means client directly uses word.
    ///
    /// Annotations are represented in the RLU using `(anno)@client`.
    /// We will not track embedded texts, binaries, or natural numbers
    /// precisely (though dependencies on zero, succ, null, cons will
    /// be tracked). 
    ///
    /// I assume dictionaries will be augmented with metadata, maintained
    /// by hand or bot, arranged for easy access via reverse lookup of the
    /// words or dedicated annotations.
    type RLU = Dict

    module RLU =

        let cSep = byte '@'
        let inline wordAtClient w c = BS.append3 w (BS.singleton cSep) c
        let blankDef = Some (Inline (BS.empty))

        let empty = Dict.empty

        /// Find direct clients with a specific prefix of a word. This is
        /// useful for some application models where we might render a word
        /// as a forum root with "replies" in a threaded model.
        let wordClientsWithPrefix (w:Symbol) (p:Prefix) (rlu:RLU) : seq<Symbol> =
            rlu |> Dict.extractPrefix (wordAtClient w p) 
                |> Dict.prependPrefix p // nop for empty prefix
                |> Dict.toSeq 
                |> Seq.map fst // ignore the value field

        /// Find all direct clients of a word.
        let wordClients (w:Word) (rlu:RLU) : seq<Symbol> =
            wordClientsWithPrefix w (BS.empty) rlu

        /// Find all direct clients of an annotation.
        let annoClients (w:Word) (rlu:RLU) : seq<Symbol> =
            wordClients (wrapParens w) rlu

        /// Find all words whose definitions failed to parse.
        let parseErrors (rlu:RLU) : seq<Symbol> =
            wordClients wEPARSE rlu

        let private defBlank : Def = Inline (BS.empty)
        let addWordDeps (c:Word) (deps:Set<Symbol>) (rlu0:RLU) : RLU =
            let addDep rlu w = Dict.add (wordAtClient w c) defBlank d 
            Set.fold addDep rlu0 deps
        let remWordDeps (c:Word) (deps:Set<Symbol>) (rlu:RLU) : RLU =
            let remDep rlu w = Dict.remove (wordAtClient w c) rlu
            Set.fold remDep rlu0 deps

        /// Update dependencies for a word.
        let updWordDeps (w:Word) (old_deps:Set<Symbol>) (new_deps:Set<Symbol>) (rlu:RLU) : RLU =
            rlu |> remWordDeps w (Set.difference old_deps new_deps)
                |> addWordDeps w (Set.difference new_deps old_deps)

        let inline private defOptDeps (defOpt : Def option) : Set<Symbol> =
            match defOpt with
            | Some def -> defDeps def
            | None -> Set.empty

        let updWordDef (w:Word) (old_def:Def option) (new_def:Def option) (rlu:RLU) =
            updWordDeps w (defOptDeps old_def) (defOptDeps new_def) rlu

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
        let update (d0:Dict) (df:Dict) (rlu:RLU) : RLU =
            Dict.diff d0 df |> Seq.fold onDictDiff rlu

        /// As update, but performs compaction incrementally, ensuring
        /// we don't have the full index in memory at once. 
        let compactingUpdate (db:Stowage) (d0:Dict) (df:Dict) (rlu0:RLU) : RLU =
            let chunkSize = 2000 // for incremental compaction
            let onChunk rlu s = Seq.fold onDictDiff rlu s |> Dict.compact db
            Dict.diff d0 df 
                |> Seq.chunkBySize chunkSize
                |> Seq.fold onChunk rlu0

        // TODO: potentially optimize initialization of the index. 

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

    /// A compacting update will control memory usage.
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
